package io.purefuncqrses.samples.raffle

import io.purefuncqrses.behavior.{Args, HasHistory, HistoryAndOptionalAggregateStateArgs, HistoryArg}
import io.purefuncqrses.samples.raffle.events.RaffleEvent

package object behavior {

  type HasRaffleHistory = HasHistory[RaffleEvent]

  type RaffleArgs = Args[RaffleEvent, RaffleState]

  type RaffleHistoryArg = HistoryArg[RaffleEvent, RaffleState]

  type RaffleHistoryAndOptionalRaffleStateArgs = HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleState]

  type State = RaffleArgs

  type HistoryState = RaffleHistoryArg

  type HistoryAndOptionalStateState = RaffleHistoryAndOptionalRaffleStateArgs

}
