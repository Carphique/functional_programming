package runner.bots

import runner.*

/**
 * Ансамблевий автопілот MISD із захисним арбітражем (Safety Veto),
 * безпечним розв'язанням нічиєї (1:1:1) та адаптацією до Backpressure.
 */
class MajorityBot(val bots: List[CyberBot]) extends CyberBot:

  private val strategicGuard = new StrategicBot()
  private val reflexFast     = new ReflexBot()

  // Попередній прогрів JIT-компілятора JVM до початку відліку таймера Engine
  private val warmupObs = Observation(
    HeroState(),
    List(TunnelSlice.empty, TunnelSlice.empty, TunnelSlice.empty, TunnelSlice.empty, TunnelSlice.empty),
    100_000_000L
  )
  bots.foreach(b => (1 to 300).foreach(_ => b.decide(warmupObs)))
  (1 to 300).foreach(_ => reflexFast.decide(warmupObs))
  (1 to 300).foreach(_ => strategicGuard.isTrajectorySafe(warmupObs, Action.KeepRunning))

  def this(seed: Long = 2026L) = this(List(
    new GreedyBot(seed + 101L),
    new ReflexBot(seed + 202L),
    new StrategicBot(seed + 303L)
  ))

  override def name: String = "MajorityEnsembleBot"

  override def decide(observation: Observation): Action =
    observation.upcoming match
      case Nil =>
        Action.KeepRunning
      case nextSlice :: _ =>
        val currentLane = observation.hero.lane

        // Адаптація до Backpressure: при падінні бюджету часу нижче 5 мс
        // перемикаємося на Zero-GC O(1) рефлекс без алокацій, щоб уникнути GC і таймаутів
        if observation.timeBudgetNanos < 5_000_000L then
          reflexFast.decide(observation)
        else
          // 1. Опитуємо тріаду MISD (один Observation -> три незалежні алгоритми)
          val labelledVotes: List[(CyberBot, Action)] =
            bots.map(bot => bot -> bot.decide(observation))

          // 2. Safety Veto ДО підрахунку голосів:
          // спершу відсіюємо смертельні дії на найближчому зрізі t+1
          val immediateSafeVotes: List[(CyberBot, Action)] =
            labelledVotes.filter { case (_, action) =>
              TunnelPhysics.isActionSafeOnSlice(nextSlice, currentLane, action)
            }

          // Додатково відсіюємо дії, які ведуть у зв'язану пастку на горизонті (якщо є вихід)
          val trajectorySafeVotes = immediateSafeVotes.filter { case (_, action) =>
            strategicGuard.isTrajectorySafe(observation, action)
          }

          val safeVotes =
            if trajectorySafeVotes.nonEmpty then trajectorySafeVotes
            else immediateSafeVotes

          if safeVotes.isEmpty then
            reflexFast.decide(observation)
          else
            // 3. Підрахунок голосів серед безпечних кандидатів
            val counts: Map[Action, Int] =
              safeVotes.map(_._2).groupBy(identity).view.mapValues(_.size).toMap

            val (winnerAction, maxVotes) = counts.maxBy(_._2)

            if maxVotes >= 2 then
              winnerAction
            else
              // 4. Безпечний тай-брейк при нічиї 1:1:1:
              // пріоритет має StrategicBot або ReflexBot, а НЕ GreedyBot
              val strategicVote = safeVotes.collectFirst { case (_: StrategicBot, act) => act }
              val reflexVote    = safeVotes.collectFirst { case (_: ReflexBot, act)    => act }

              strategicVote
                .orElse(reflexVote)
                .orElse(safeVotes.headOption.map(_._2))
                .getOrElse(Action.KeepRunning)