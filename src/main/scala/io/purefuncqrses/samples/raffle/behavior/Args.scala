package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory

sealed trait Args {
  def getRaffleHistory: RaffleHistory
  def getOptionalRaffleState: Option[RaffleState]
}

case class History_Args(raffleHistory: RaffleHistory) extends Args {
  override def getRaffleHistory: RaffleHistory = raffleHistory
  override def getOptionalRaffleState: Option[RaffleState] =
    sys.error("Cannot extract optional raffle state from history argument")
}

case class History_And_OptionalState_Args(raffleHistory: RaffleHistory, optionalRaffleState: Option[RaffleState]) extends Args {
  override def getRaffleHistory: RaffleHistory = raffleHistory
  override def getOptionalRaffleState: Option[RaffleState] = optionalRaffleState
}


