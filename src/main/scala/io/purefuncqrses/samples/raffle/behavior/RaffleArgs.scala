package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory

//sealed trait Args {
//  def getRaffleHistory: RaffleHistory
//
//  def getOptionalRaffleState: Option[RaffleState]
//}
//
//case class HistoryArg(raffleHistory: RaffleHistory) extends Args {
//  override def getRaffleHistory: RaffleHistory = raffleHistory
//
//  override def getOptionalRaffleState: Option[RaffleState] =
//    sys.error("Cannot extract optional raffle state from history argument")
//}
//
//case class HistoryAndOptionalStateArgs(raffleHistory: RaffleHistory, optionalRaffleState: Option[RaffleState]) extends Args {
//  override def getRaffleHistory: RaffleHistory = raffleHistory
//
//  override def getOptionalRaffleState: Option[RaffleState] = optionalRaffleState
//}


/*


trait HasRaffleHistory {

  def getHistory: RaffleHistory

}

sealed trait RaffleArgs extends HasRaffleHistory {

  def getOptionalAggregateState: Option[RaffleState]

}

case class RaffleHistoryArg(raffleHistory: RaffleHistory) extends RaffleArgs {

  override def getHistory: RaffleHistory = raffleHistory

  override def getOptionalAggregateState: Option[RaffleState] =
    sys.error("Cannot extract optional raffle state from history argument")

}

case class RaffleHistoryAndOptionalRaffleStateArgs(raffleHistory: RaffleHistory, optionalRaffleState: Option[RaffleState]) extends RaffleArgs {

  override def getHistory: RaffleHistory = raffleHistory

  override def getOptionalAggregateState: Option[RaffleState] = optionalRaffleState

}

*/
