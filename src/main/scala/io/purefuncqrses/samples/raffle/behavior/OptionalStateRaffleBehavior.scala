package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.behavior.HistoryArg
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class OptionalStateRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistoryState, ?[_]]]
  extends OptimizedRaffleBehavior[RaffleHistoryState, M] {

  import implicitFailureF._

  import implicitStateF._


  var currentOptionalRaffleAggregate: Option[RaffleAggregate] = None


  override protected def setStateFromArgs(args: RaffleHistoryAndOptionalRaffleAggregateArgs): M[Unit] = {
    val newRaffleHistory: RaffleHistory = args.getHistory
    val newOptionalRaffleAggregate: Option[RaffleAggregate] = args.getOptionalAggregate
    currentOptionalRaffleAggregate = newOptionalRaffleAggregate
    val state: RaffleHistoryState = HistoryArg[RaffleEvent](newRaffleHistory)
    write {
      state
    }
  }

  override protected def handlerTemplate[Cmd](condition: RaffleHistoryAndOptionalRaffleAggregateArgs => Boolean, block: RaffleHistoryAndOptionalRaffleAggregateArgs => M[Unit]): Handler[Cmd, M] = { command =>
    read(()) flatMap { state =>
      val args: RaffleHistoryAndOptionalRaffleAggregateArgs =
        mkRaffleHistoryAndOptionalRaffleAggregateArgs(state.history, currentOptionalRaffleAggregate)
      val currentHistory: RaffleHistory = args.getHistory
      if (condition(args)) {
        block(args)
      } else {
        failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
      }
    }
  }


}
