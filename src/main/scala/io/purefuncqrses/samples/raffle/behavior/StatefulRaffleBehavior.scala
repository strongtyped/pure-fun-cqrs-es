package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import shapeless.{HList, HNil}

import scala.language.higherKinds

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitRaffleHistoryState1F._

  var currentOptionalRaffleState: Option[RaffleState] = None

  override protected def setState(hList: HList): M[Unit] = {
    val newOptionalRaffleState: Option[RaffleState] = hList.asInstanceOf[shapeless.::[Option[RaffleState], shapeless.::[RaffleHistory, HNil]]].head
    val newRaffleHistory: RaffleHistory = hList.asInstanceOf[shapeless.::[RaffleHistory, shapeless.::[RaffleHistory, HNil]]].tail.asInstanceOf[shapeless.::[RaffleHistory, HNil]].head
    this.currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  override protected def raffleCommandHandlerTemplate(command: RaffleCommand, commandHandlerBody: (RaffleCommand, HList) => M[Unit]): M[Unit] = {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      commandHandlerBody(command, currentRaffleHistory :: currentOptionalRaffleState :: HNil)
    }
  }

  override protected def raffleCommandWithNameHandlerTemplate(command: RaffleCommandWithName, commandWithNameHandlerBody: (RaffleCommandWithName, HList) => M[Unit]): M[Unit] = {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      commandWithNameHandlerBody(command, currentRaffleHistory :: currentOptionalRaffleState :: HNil)
    }
  }

}
