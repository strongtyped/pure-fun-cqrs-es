package io.purefuncqrses.samples.raffle

import io.purefuncqrses.behavior.{Args, HasHistory, HistoryAndOptionalAggregateArgs, HistoryArg}
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.events.RaffleEvent

package object behavior {

  type HasRaffleHistory = HasHistory[RaffleEvent]

  type RaffleArgs = Args[RaffleEvent]


  type RaffleHistoryArg = HistoryArg[RaffleEvent]

  val mkRaffleHistoryArg: (RaffleHistory) => RaffleHistoryArg =
    (raffleHistory: RaffleHistory) =>
      HistoryArg[RaffleEvent](raffleHistory)

  type RaffleHistoryAndOptionalRaffleAggregateArgs = HistoryAndOptionalAggregateArgs[RaffleEvent, RaffleAggregate]

  val mkRaffleHistoryAndOptionalRaffleAggregateArgs: (RaffleHistory, Option[RaffleAggregate]) => RaffleHistoryAndOptionalRaffleAggregateArgs =
    (raffleHistory: RaffleHistory, optionalRaffleAggregate: Option[RaffleAggregate]) =>
      HistoryAndOptionalAggregateArgs[RaffleEvent, RaffleAggregate](raffleHistory, optionalRaffleAggregate)


  type RaffleState = RaffleArgs

  type RaffleHistoryState = RaffleHistoryArg

  type RaffleHistoryAndOptionalRaffleAggregateState = RaffleHistoryAndOptionalRaffleAggregateArgs

}
