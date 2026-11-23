package runner.bots

import runner.*

/**
 * Стратегічний бот із графом досяжності на кілька зрізів уперед (Zero-GC)
 * та динамічною адаптацією до timeBudgetNanos (Backpressure).
 */
class StrategicBot(seed: Long = 303L) extends CyberBot:

  override def name: String = "StrategicBot"

  private def maxReachableScore(lane: Lane, slices: List[TunnelSlice], depth: Int): Int =
    if depth <= 0 then 0
    else
      slices match
        case Nil => 0
        case slice :: rest =>
          val sRun   = evalMove(lane, Action.KeepRunning, slice, rest, depth)
          val sDuck  = evalMove(lane, Action.Duck, slice, rest, depth)
          val sJump  = evalMove(lane, Action.Jump, slice, rest, depth)
          val sLeft  = evalMove(lane, Action.MoveLeft, slice, rest, depth)
          val sRight = evalMove(lane, Action.MoveRight, slice, rest, depth)
          Math.max(sRun, Math.max(sDuck, Math.max(sJump, Math.max(sLeft, sRight))))

  private def evalMove(
      currentLane: Lane,
      action: Action,
      slice: TunnelSlice,
      rest: List[TunnelSlice],
      depth: Int
  ): Int =
    if !TunnelPhysics.isActionSafeOnSlice(slice, currentLane, action) then
      -1_000_000
    else
      val nextLane = action match
        case Action.MoveLeft  => currentLane.left.getOrElse(currentLane)
        case Action.MoveRight => currentLane.right.getOrElse(currentLane)
        case _                => currentLane

      val nextStance = action match
        case Action.Jump => Stance.Jumping
        case Action.Duck => Stance.Ducking
        case _           => Stance.Running

      val future = maxReachableScore(nextLane, rest, depth - 1)
      if future <= -500_000 then
        -1_000_000
      else
        val coins = TunnelPhysics.coinsCollectedAt(slice, nextLane, nextStance)
        val centerBonus = if nextLane == Lane.Center then 2 else 0
        val penalty = if action == Action.KeepRunning then 0 else -1
        coins * 10 + centerBonus + penalty + future

  private def adaptiveDepth(timeBudgetNanos: Long): Int =
    if timeBudgetNanos < 20_000L then 1
    else if timeBudgetNanos < 150_000L then 2
    else if timeBudgetNanos < 1_000_000L then 3
    else 5

  /**
   * Перевіряє, чи не веде обрана дія у зв'язану пастку на горизонті планування.
   */
  def isTrajectorySafe(observation: Observation, action: Action): Boolean =
    observation.upcoming match
      case Nil => true
      case firstSlice :: restSlices =>
        val depth = adaptiveDepth(observation.timeBudgetNanos)
        evalMove(observation.hero.lane, action, firstSlice, restSlices, depth) > -500_000

  override def decide(observation: Observation): Action =
    observation.upcoming match
      case Nil =>
        Action.KeepRunning
      case firstSlice :: restSlices =>
        val lane = observation.hero.lane
        val depth = adaptiveDepth(observation.timeBudgetNanos)

        val sRun   = evalMove(lane, Action.KeepRunning, firstSlice, restSlices, depth)
        val sDuck  = evalMove(lane, Action.Duck, firstSlice, restSlices, depth)
        val sJump  = evalMove(lane, Action.Jump, firstSlice, restSlices, depth)
        val sLeft  = evalMove(lane, Action.MoveLeft, firstSlice, restSlices, depth)
        val sRight = evalMove(lane, Action.MoveRight, firstSlice, restSlices, depth)

        val bestScore = Math.max(sRun, Math.max(sDuck, Math.max(sJump, Math.max(sLeft, sRight))))

        if bestScore <= -500_000 then
          if TunnelPhysics.isActionSafeOnSlice(firstSlice, lane, Action.KeepRunning) then Action.KeepRunning
          else if TunnelPhysics.isActionSafeOnSlice(firstSlice, lane, Action.Duck) then Action.Duck
          else if TunnelPhysics.isActionSafeOnSlice(firstSlice, lane, Action.Jump) then Action.Jump
          else if TunnelPhysics.isActionSafeOnSlice(firstSlice, lane, Action.MoveLeft) then Action.MoveLeft
          else if TunnelPhysics.isActionSafeOnSlice(firstSlice, lane, Action.MoveRight) then Action.MoveRight
          else Action.KeepRunning
        else if sRun == bestScore then Action.KeepRunning
        else if sDuck == bestScore then Action.Duck
        else if sJump == bestScore then Action.Jump
        else if sLeft == bestScore then Action.MoveLeft
        else Action.MoveRight