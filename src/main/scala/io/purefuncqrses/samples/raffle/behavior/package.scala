package io.purefuncqrses.samples.raffle

import io.purefuncqrses.behavior.{Args, HasHistory, HistoryAndOptionalAggregateStateArgs, HistoryArg}
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.events.RaffleEvent

package object behavior {

  type HasRaffleHistory = HasHistory[RaffleEvent]

  type RaffleArgs = Args[RaffleEvent]

  type RaffleHistoryArg = HistoryArg[RaffleEvent]

  val raffleHistoryArg =
    (raffleHistory: RaffleHistory) =>
      HistoryArg[RaffleEvent](raffleHistory)

  type RaffleHistoryAndOptionalRaffleAggregateStateArgs = HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleAggregateState]

  val raffleHistoryAndOptionalRaffleAggregateStateArgs =
    (raffleHistory: RaffleHistory, optionalRaffleAggregateState: Option[RaffleAggregateState]) =>
      HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleAggregateState](raffleHistory, optionalRaffleAggregateState)

  type RaffleState = RaffleArgs

  type RaffleHistoryState = RaffleHistoryArg

  type RaffleHistoryAndOptionalStateState = RaffleHistoryAndOptionalRaffleAggregateStateArgs

}
