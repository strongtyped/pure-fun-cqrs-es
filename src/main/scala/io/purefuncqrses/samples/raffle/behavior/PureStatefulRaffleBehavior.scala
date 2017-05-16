package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.{Handler, State}
import io.purefuncqrses.features._
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class PureStatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[State, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitFailureF._

  import implicitRaffleHistoryState1F._

//  private val implicitOptionalRaffleStateState2F = implicitly[State2F[Option[RaffleState], M]]
//
//  import implicitOptionalRaffleStateState2F._


  override protected def setState(args: Args): M[Unit] = args match {
    case History_And_OptionalState_Args(_, _) =>
      val newRaffleHistory: RaffleHistory = args.getRaffleHistory
      val newOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
        setState {
          History_And_OptionalState_Args(newRaffleHistory, newOptionalRaffleState)
        }
    case args =>
      failure(new IllegalStateException(s"$args are not history optional state arguments"))
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    getState(()) flatMap {
      case History_And_OptionalState_Args(currentRaffleHistory, currentOptionalRaffleState) =>
        handlerBody(command, History_And_OptionalState_Args(currentRaffleHistory, currentOptionalRaffleState))
      case args =>
        failure(new IllegalStateException(s"$args are not history optional state arguments"))
      }
  }

}
