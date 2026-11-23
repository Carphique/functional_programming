package runner.bots

import runner.*

/**
 * Чиста функціональна модель фізики тунелю (Zero-GC, без мутацій та побічних ефектів).
 * Повністю відповідає правилам переходів у Engine.scala.
 */
object TunnelPhysics:

  val AllActions: List[Action] =
    List(Action.KeepRunning, Action.Duck, Action.Jump, Action.MoveLeft, Action.MoveRight)

  def transition(lane: Lane, action: Action): Option[(Lane, Stance)] =
    action match
      case Action.MoveLeft    => lane.left.map(l => (l, Stance.Running))
      case Action.MoveRight   => lane.right.map(l => (l, Stance.Running))
      case Action.Jump        => Some((lane, Stance.Jumping))
      case Action.Duck        => Some((lane, Stance.Ducking))
      case Action.KeepRunning => Some((lane, Stance.Running))

  def isPositionSafe(slice: TunnelSlice, lane: Lane, stance: Stance): Boolean =
    stance match
      case Stance.Running =>
        !slice.hasObstacleAt(lane, Height.Low) && !slice.hasObstacleAt(lane, Height.Mid)
      case Stance.Jumping =>
        !slice.hasObstacleAt(lane, Height.High)
      case Stance.Ducking =>
        !slice.hasObstacleAt(lane, Height.Low)

  /**
   * Надшвидка перевірка безпеки дії без створення Option/Tuple у купі (Zero-GC).
   */
  def isActionSafeOnSlice(slice: TunnelSlice, currentLane: Lane, action: Action): Boolean =
    action match
      case Action.KeepRunning =>
        !slice.hasObstacleAt(currentLane, Height.Low) && !slice.hasObstacleAt(currentLane, Height.Mid)
      case Action.Duck =>
        !slice.hasObstacleAt(currentLane, Height.Low)
      case Action.Jump =>
        !slice.hasObstacleAt(currentLane, Height.High)
      case Action.MoveLeft =>
        currentLane match
          case Lane.Right  => !slice.hasObstacleAt(Lane.Center, Height.Low) && !slice.hasObstacleAt(Lane.Center, Height.Mid)
          case Lane.Center => !slice.hasObstacleAt(Lane.Left, Height.Low) && !slice.hasObstacleAt(Lane.Left, Height.Mid)
          case Lane.Left   => false
      case Action.MoveRight =>
        currentLane match
          case Lane.Left   => !slice.hasObstacleAt(Lane.Center, Height.Low) && !slice.hasObstacleAt(Lane.Center, Height.Mid)
          case Lane.Center => !slice.hasObstacleAt(Lane.Right, Height.Low) && !slice.hasObstacleAt(Lane.Right, Height.Mid)
          case Lane.Right  => false

  def coinsCollectedAt(slice: TunnelSlice, lane: Lane, stance: Stance): Int =
    stance match
      case Stance.Running =>
        slice.coinAt(lane, Height.Low).getOrElse(0) +
        slice.coinAt(lane, Height.Mid).getOrElse(0)
      case Stance.Jumping =>
        slice.coinAt(lane, Height.High).getOrElse(0)
      case Stance.Ducking =>
        slice.coinAt(lane, Height.Low).getOrElse(0)