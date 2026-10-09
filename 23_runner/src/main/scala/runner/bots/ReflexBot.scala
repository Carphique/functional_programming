package runner.bots

import runner.*

/** Рефлексивний бот: аналізує upcoming.head і кілька наступних зрізів, обирає безпечну дію. */
class ReflexBot(seed: Long = 202L) extends CyberBot:

  private val Horizon = 4

  private val preference: List[Action] =
    List(Action.KeepRunning, Action.Duck, Action.Jump, Action.MoveLeft, Action.MoveRight)

  override def name: String = "ReflexBot"

  private def outcome(lane: Lane, action: Action): Option[(Lane, Stance)] =
    action match
      case Action.MoveLeft    => lane.left.map(l => (l, Stance.Running))
      case Action.MoveRight   => lane.right.map(l => (l, Stance.Running))
      case Action.Jump        => Some((lane, Stance.Jumping))
      case Action.Duck        => Some((lane, Stance.Ducking))
      case Action.KeepRunning => Some((lane, Stance.Running))

  private def isSafe(slice: TunnelSlice, lane: Lane, stance: Stance): Boolean =
    stance match
      case Stance.Running =>
        !slice.hasObstacleAt(lane, Height.Low) && !slice.hasObstacleAt(lane, Height.Mid)
      case Stance.Jumping =>
        !slice.hasObstacleAt(lane, Height.High)
      case Stance.Ducking =>
        !slice.hasObstacleAt(lane, Height.Low)

  private def survives(lane: Lane, slices: List[TunnelSlice]): Boolean =
    slices match
      case Nil => true
      case slice :: rest =>
        preference.exists { action =>
          outcome(lane, action).exists { case (newLane, stance) =>
            isSafe(slice, newLane, stance) && survives(newLane, rest)
          }
        }

  override def decide(observation: Observation): Action =
    val lane = observation.hero.lane

    observation.upcoming.take(Horizon) match
      case Nil => Action.KeepRunning
      case head :: rest =>
        val safeNow = preference.filter { action =>
          outcome(lane, action).exists { case (newLane, stance) =>
            isSafe(head, newLane, stance)
          }
        }

        val farSighted = safeNow.find { action =>
          outcome(lane, action).exists { case (newLane, _) =>
            survives(newLane, rest)
          }
        }

        farSighted.orElse(safeNow.headOption).getOrElse(Action.KeepRunning)