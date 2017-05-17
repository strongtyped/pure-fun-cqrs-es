package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.{Handler, HandlerBody}
import io.purefuncqrses.behavior.{HistoryAndOptionalAggregateStateArgs, HistoryArg}
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class OptionalStateRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[HistoryState, ?[_]]]
  extends OptimizedRaffleBehavior[HistoryState, M] {

  import implicitFailureF._

  import implicitStateF._


  var currentOptionalRaffleState: Option[RaffleState] = None


  override protected def setState(args: RaffleHistoryAndOptionalRaffleStateArgs): M[Unit] = {
    val newRaffleHistory: RaffleHistory = args.getHistory
    val newOptionalRaffleState: Option[RaffleState] = args.getOptionalAggregateState
    this.currentOptionalRaffleState = newOptionalRaffleState
    val state: HistoryState = HistoryArg[RaffleEvent, RaffleState](newRaffleHistory)
    write {
      state
    }
  }


  override protected def handlerTemplate[Cmd](condition: RaffleHistoryAndOptionalRaffleStateArgs => Boolean, newArgs: RaffleHistoryAndOptionalRaffleStateArgs => RaffleHistoryAndOptionalRaffleStateArgs): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      case state =>
        val args: RaffleHistoryAndOptionalRaffleStateArgs = HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleState](state.raffleHistory, currentOptionalRaffleState)
        val currentHistory: RaffleHistory = args.getHistory
        println(s"\ncurrent history = $currentHistory")
        if (condition(args)) {
          setState(newArgs(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
    }
  }

}
