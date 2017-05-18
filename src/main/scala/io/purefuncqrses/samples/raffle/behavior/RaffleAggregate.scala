package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.samples.raffle.id.RaffleId

sealed trait RaffleAggregate {
  def raffleId: RaffleId
}

case class Open(raffleId: RaffleId, participants: List[String] = List())
  extends RaffleAggregate

case class Closed(raffleId: RaffleId, winner: String)
  extends RaffleAggregate

