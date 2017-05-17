package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class OptionalStateRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]]]
  extends OptimizedRaffleBehavior[M] {

  import implicitFailureF._

  import implicitStateF._


  var currentOptionalRaffleState: Option[RaffleState] = None


  override protected def setState(args: Args): M[Unit] = args match {
    case HistoryAndOptionalStateArgs(_, _) =>
      val newRaffleHistory: RaffleHistory = args.getRaffleHistory
      val newOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
      this.currentOptionalRaffleState = newOptionalRaffleState
      val state: State = HistoryArg(newRaffleHistory)
      write {
        state
      }
    case _ =>
      failure(new IllegalStateException(s"$args are not 'history and optional state' arguments"))
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      case state: HistoryArg =>
        handlerBody(command, HistoryAndOptionalStateArgs(state.raffleHistory, currentOptionalRaffleState))
      case state =>
        failure(new IllegalStateException(s"$state is not a 'history' state"))
    }
  }

}
