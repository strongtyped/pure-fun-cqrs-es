package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.samples.raffle.id.RaffleId

sealed trait RaffleState {
  def raffleId: RaffleId
}

case class OpenState(raffleId: RaffleId, participants: List[String] = List())
  extends RaffleState

case class ClosedState(raffleId: RaffleId, winner: String)
  extends RaffleState
