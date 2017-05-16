package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.features.{FailureF, RunF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.Args
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{RaffleCommands, RaffleHistory}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.behavior.Behavior.{State, seq}
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

abstract class AbstractRaffleApp[M[+ _] : SuccessF : FailureF : State1F[State, ?[_]] : RunF] {

  protected val implicitRunF: RunF[M] = implicitly[RunF[M]]

  import implicitRunF.{Input, run}

  protected val raffleBehavior: Behavior[RaffleCommand, RaffleEvent, RaffleId, M]

  protected val raffleCommands: RaffleCommands =
    seq(
      CreateRaffleCommand,
      AddParticipantCommand("John"),
      //      CreateRaffleAddingParticipantCommand("John"),
      AddParticipantCommand("Paul"),
      AddParticipantCommand("George"),
      AddParticipantCommand("Ringo"),
      RemoveParticipantCommand("Paul"),
      SelectWinnerCommand
    )

  protected val input: Input

  protected type Output = (Args, Unit)

  protected def output(): Output = {
    val raffle: M[Unit] = raffleBehavior.handlerForEach(raffleCommands)
    run(raffle)(input)
  }

  protected def raffleHistory: RaffleHistory = output()._1.getRaffleHistory

  def runApp() {
    println("\n" + raffleHistory)
  }

}
