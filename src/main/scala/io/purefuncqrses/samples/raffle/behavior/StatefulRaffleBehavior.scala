package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import shapeless.{HList, HNil}

import scala.language.higherKinds

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitRaffleHistoryState1F._

  var currentOptionalRaffleState: Option[RaffleState] = None


  override protected def setState(hList: HList): M[Unit] = {
    val newRaffleHistory: RaffleHistory = hList._1
    val newOptionalRaffleState: Option[RaffleState] = hList._2
    this.currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  override protected def handlerTemplate[Cmd](command: Cmd, handlerBody: (Cmd, HList) => M[Unit]): M[Unit] = {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      handlerBody(command, currentRaffleHistory :: currentOptionalRaffleState :: HNil)
    }
  }

}
