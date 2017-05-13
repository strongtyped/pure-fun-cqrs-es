package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{RaffleCommands, RaffleHistory}
import io.purefuncqrses.samples.raffle.commands.{AddParticipantCommand, CreateRaffleCommand, RemoveParticipantCommand, SelectWinnerCommand}
import io.purefuncqrses.behavior.Behavior.seq

import scala.language.higherKinds

abstract class AbstractRaffleApp[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]] : RunF] {

  protected val implicitRunF: RunF[M] = implicitly[RunF[M]]

  import implicitRunF.{Input, run}

  protected val raffleBehavior: AbstractRaffleBehavior[M]

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

  protected type Output

  protected def output(): Output = {
    val raffle: M[Unit] = raffleBehavior.handle(raffleCommands)
    run(raffle)(input)
  }

  protected val raffleHistory: RaffleHistory

  def runApp() {
    println("\n" + raffleHistory.last)
  }

}
