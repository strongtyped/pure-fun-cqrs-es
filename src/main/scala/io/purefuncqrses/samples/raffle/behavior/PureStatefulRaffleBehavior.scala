package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.util.Util._
import io.purefuncqrses.features._
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}
import shapeless.{HList, HNil}

import scala.language.higherKinds

class PureStatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]] : State2F[Option[RaffleState], ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitRaffleHistoryState1F._

  private val implicitOptionalRaffleStateState2F = implicitly[State2F[Option[RaffleState], M]]

  import implicitOptionalRaffleStateState2F._


  override protected def setState(hList: HList): M[Unit] = {
    val newRaffleHistory: RaffleHistory = hList._1
    val newOptionalRaffleState: Option[RaffleState] = hList._2
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
        handlerBody(command, currentRaffleHistory :: currentOptionalRaffleState :: HNil)
      }
    }
  }

}
