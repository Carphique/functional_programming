package runner.bots

import runner.*

/**
 * Жадібний бот-збирач монет (без Random і мутабельного стану).
 * Обирає безпечну дію, яка максимізує миттєвий збір монет на найближчому зрізі
 * без самогубних зіткнень із перешкодами.
 */
class GreedyBot(seed: Long = 101L) extends CyberBot:

  override def name: String = "GreedyBot"

  override def decide(observation: Observation): Action =
    observation.upcoming match
      case Nil =>
        Action.KeepRunning
      case nextSlice :: futureSlices =>
        val currentLane = observation.hero.lane

        val safeActions = TunnelPhysics.AllActions.filter { action =>
          TunnelPhysics.isActionSafeOnSlice(nextSlice, currentLane, action)
        }

        if safeActions.isEmpty then
          Action.KeepRunning
        else
          val secondSliceOpt = futureSlices.headOption
          val scoredActions = safeActions.zipWithIndex.map { case (action, priorityIdx) =>
            val (nextLane, nextStance) = TunnelPhysics.transition(currentLane, action).get
            val immediateCoins = TunnelPhysics.coinsCollectedAt(nextSlice, nextLane, nextStance)

            val futureHint = secondSliceOpt.map { s2 =>
              Height.values.flatMap(h => s2.coinAt(nextLane, h)).sum / 4
            }.getOrElse(0)

            val totalValue = immediateCoins * 10 + futureHint
            (action, totalValue, -priorityIdx)
          }

          scoredActions.maxBy { case (_, value, tieBreaker) => (value, tieBreaker) }._1