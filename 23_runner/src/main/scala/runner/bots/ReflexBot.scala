package runner.bots

import runner.*

/**
 * Рефлексивний бот-охоронець (складність O(1), Zero-GC без алокацій).
 * Перевіряє виключно найближчий зріз (observation.upcoming.head)
 * з урахуванням фізики висот (Low, Mid, High), стійки та меж тунелю.
 */
class ReflexBot(seed: Long = 202L) extends CyberBot:

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
    observation.upcoming match
      case Nil =>
        Action.KeepRunning
      case nextSlice :: _ =>
        val lane = observation.hero.lane
        if TunnelPhysics.isActionSafeOnSlice(nextSlice, lane, Action.KeepRunning) then Action.KeepRunning
        else if TunnelPhysics.isActionSafeOnSlice(nextSlice, lane, Action.Duck) then Action.Duck
        else if TunnelPhysics.isActionSafeOnSlice(nextSlice, lane, Action.Jump) then Action.Jump
        else if TunnelPhysics.isActionSafeOnSlice(nextSlice, lane, Action.MoveLeft) then Action.MoveLeft
        else if TunnelPhysics.isActionSafeOnSlice(nextSlice, lane, Action.MoveRight) then Action.MoveRight
        else Action.KeepRunning