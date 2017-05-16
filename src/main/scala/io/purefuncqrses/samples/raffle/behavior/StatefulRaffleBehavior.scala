package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitFailureF._

  import implicitRaffleHistoryState1F._

  var currentOptionalRaffleState: Option[RaffleState] = None


  override protected def setState(args: Args): M[Unit] = args match {
    case History_And_OptionalState_Args(_, _) =>
      val newRaffleHistory: RaffleHistory = args.getRaffleHistory
      val newOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
      this.currentOptionalRaffleState = newOptionalRaffleState
      setState1 {
        newRaffleHistory
      }
    case _ =>
      failure(new IllegalStateException(s"$args are not history optional state arguments"))
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      handlerBody(command, History_And_OptionalState_Args(currentRaffleHistory, currentOptionalRaffleState))
    }
  }

}
