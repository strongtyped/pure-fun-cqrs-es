package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.features._
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class PureStatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]] : State2F[Option[RaffleState], ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitRaffleHistoryState1F._

  private val implicitOptionalRaffleStateState2F = implicitly[State2F[Option[RaffleState], M]]

  import implicitOptionalRaffleStateState2F._


  override protected def setState(args: Args): M[Unit] = {
    val newRaffleHistory: RaffleHistory = args.getRaffleHistory
    val newOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    setState2 {
      newOptionalRaffleState
    } flatSeq {
      setState1 {
        newRaffleHistory
      }
    }
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      getState2(()) flatMap { currentOptionalRaffleState =>
        handlerBody(command, History_And_OptionalState_Args(currentRaffleHistory, currentOptionalRaffleState))
      }
    }
  }

}
