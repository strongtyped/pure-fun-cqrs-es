package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.{Handler, State}
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[State, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitFailureF._

  import implicitRaffleHistoryState1F._

  var currentOptionalRaffleState: Option[RaffleState] = None


  override protected def setState(args: Args): M[Unit] = args match {
    case History_And_OptionalState_Args(_, _) =>
      val newRaffleHistory: RaffleHistory = args.getRaffleHistory
      val newOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
      this.currentOptionalRaffleState = newOptionalRaffleState
      setState {
        History_And_OptionalState_Args(newRaffleHistory, newOptionalRaffleState)
      }
    case _ =>
      failure(new IllegalStateException(s"$args is not a history argument"))
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    getState(()) flatMap {
      case History_And_OptionalState_Args(currentRaffleHistory, _) =>
      handlerBody(command, History_And_OptionalState_Args(currentRaffleHistory, currentOptionalRaffleState))
      case args =>
        failure(new IllegalStateException(s"$args is not a history argument"))
    }
  }

}
