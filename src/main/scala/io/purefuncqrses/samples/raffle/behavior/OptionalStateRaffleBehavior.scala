package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.{Handler, HandlerBody}
import io.purefuncqrses.behavior.{HistoryAndOptionalAggregateStateArgs, HistoryArg}
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class OptionalStateRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistoryState, ?[_]]]
  extends OptimizedRaffleBehavior[RaffleHistoryState, M] {

  import implicitFailureF._

  import implicitStateF._


  var currentOptionalRaffleState: Option[RaffleAggregateState] = None


  override protected def setStateFromArgs(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): M[Unit] = {
    val newRaffleHistory: RaffleHistory = args.getHistory
    val newOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    this.currentOptionalRaffleState = newOptionalRaffleState
    val state: RaffleHistoryState = HistoryArg[RaffleEvent](newRaffleHistory)
    write {
      state
    }
  }


  override protected def handlerTemplate[Cmd](condition: RaffleHistoryAndOptionalRaffleAggregateStateArgs => Boolean, newArgs: RaffleHistoryAndOptionalRaffleAggregateStateArgs => RaffleHistoryAndOptionalRaffleAggregateStateArgs): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      case state =>
        val args: RaffleHistoryAndOptionalRaffleAggregateStateArgs = HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleAggregateState](state.history, currentOptionalRaffleState)
        val currentHistory: RaffleHistory = args.getHistory
        println(s"\ncurrent history = $currentHistory")
        if (condition(args)) {
          setStateFromArgs(newArgs(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
    }
  }

}
