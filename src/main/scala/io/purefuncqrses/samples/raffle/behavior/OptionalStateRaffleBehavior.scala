package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class OptionalStateRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[HistoryState, ?[_]]]
  extends OptimizedRaffleBehavior[HistoryState, M] {

  import implicitStateF._


  var currentOptionalRaffleState: Option[RaffleState] = None

  //
  // templates
  //
  override protected def setState(args: HistoryAndOptionalStateArgs): M[Unit] = {
    val newRaffleHistory: RaffleHistory = args.getRaffleHistory
    val newOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    this.currentOptionalRaffleState = newOptionalRaffleState
    val newState: HistoryState = HistoryArg(newRaffleHistory)
    write {
      newState
    }
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[HistoryAndOptionalStateArgs, Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      case state =>
        handlerBody(command, HistoryAndOptionalStateArgs(state.raffleHistory, currentOptionalRaffleState))
    }
  }

}
