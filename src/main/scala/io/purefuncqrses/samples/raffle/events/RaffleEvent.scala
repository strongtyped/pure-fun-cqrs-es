package io.purefuncqrses.samples.raffle.events

import java.time.OffsetDateTime

import io.purefuncqrses.samples.raffle.id.RaffleId

sealed trait RaffleEvent {
  def raffleId: RaffleId
}

case class RaffleCreatedEvent(raffleId: RaffleId) extends RaffleEvent

case class ParticipantAddedEvent(name: String, raffleId: RaffleId) extends RaffleEvent

case class ParticipantRemovedEvent(name: String, raffleId: RaffleId) extends RaffleEvent

case class WinnerSelectedEvent(winner: String, date: OffsetDateTime, raffleId: RaffleId) extends RaffleEvent
