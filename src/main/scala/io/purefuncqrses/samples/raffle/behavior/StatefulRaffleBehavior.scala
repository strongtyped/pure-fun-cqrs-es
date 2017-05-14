package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{PartialRaffleCommandHandler, RaffleHistory}

import scala.language.higherKinds

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitRaffleHistoryState1F._

  var currentOptionalRaffleState: Option[RaffleState] = None

  override protected def setState(newOptionalRaffleState: Option[RaffleState], newRaffleHistory: RaffleHistory): M[Unit] = {
    this.currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  override protected def raffleCommandHandlerTemplate(command: RaffleCommand, commandHandlerBody: (RaffleCommand, RaffleHistory, Option[RaffleState]) => M[Unit]): M[Unit] = {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      commandHandlerBody(command, currentRaffleHistory, currentOptionalRaffleState)
    }
  }

  override protected def raffleCommandWithNameHandlerTemplate(command: RaffleCommandWithName, commandWithNameHandlerBody: (RaffleCommandWithName, RaffleHistory, Option[RaffleState]) => M[Unit]): M[Unit] = {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      commandWithNameHandlerBody(command, currentRaffleHistory, currentOptionalRaffleState)
    }
  }

}
