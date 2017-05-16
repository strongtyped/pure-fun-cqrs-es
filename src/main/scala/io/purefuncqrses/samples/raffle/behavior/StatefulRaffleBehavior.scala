package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}
import shapeless.{HList, HNil}

import scala.language.higherKinds

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitRaffleHistoryState1F._

  var currentOptionalRaffleState: Option[RaffleState] = None


  override protected def setState(hList: HList): M[Unit] = {
    val newRaffleHistory: RaffleHistory = hList.getRaffleHistory
    val newOptionalRaffleState: Option[RaffleState] = hList.getOptionalRaffleState
    this.currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      handlerBody(command, currentRaffleHistory :: currentOptionalRaffleState :: HNil)
    }
  }

}
