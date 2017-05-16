package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.{AbstractRaffleBehavior, State}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{RaffleCommands, RaffleHistory}
import io.purefuncqrses.samples.raffle.commands.{AddParticipantCommand, CreateRaffleCommand, RemoveParticipantCommand, SelectWinnerCommand}
import io.purefuncqrses.behavior.Behavior.seq

import scala.language.higherKinds

abstract class AbstractRaffleApp[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]] : RunF] {

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

  protected def input: Input

  protected type Output = (State, Unit)

  protected def output: Output = {
    val raffle: M[Unit] = raffleBehavior.handlerForEach(raffleCommands)
    run(raffle)(input)
  }

  protected def state: State = output._1

  def runApp() {
    val result = state
    println("\n================================================================================")
    println(result)
    println("================================================================================\n")
  }

}
