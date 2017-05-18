package io.purefuncqrses.samples.raffle

import io.purefuncqrses.behavior.Behavior.History
import io.purefuncqrses.behavior.{Args, HasHistory, HistoryAndOptionalAggregateStateArgs, HistoryArg}
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.events.RaffleEvent

package object behavior {

  type HasRaffleHistory = HasHistory[RaffleEvent]

  type RaffleArgs = Args[RaffleEvent, RaffleAggregateState]

  type RaffleHistoryArg = HistoryArg[RaffleEvent, RaffleAggregateState]

  val raffleHistoryArg = (raffleHistory: RaffleHistory) =>
    HistoryArg[RaffleEvent, RaffleAggregateState](raffleHistory)

  type RaffleHistoryAndOptionalRaffleStateArgs = HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleAggregateState]

  val raffleHistoryAndOptionalRaffleStateArgs= (raffleHistory: RaffleHistory, optionalRaffleAggregateState: Option[RaffleAggregateState]) =>
    HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleAggregateState](raffleHistory, optionalRaffleAggregateState)

  type RaffleState = RaffleArgs

  type HistoryState = RaffleHistoryArg

  type HistoryAndOptionalStateState = RaffleHistoryAndOptionalRaffleStateArgs

}
