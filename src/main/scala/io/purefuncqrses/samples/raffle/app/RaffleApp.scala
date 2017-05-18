package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.{RaffleArgs, RaffleState}
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleCommands
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.behavior.Behavior.seq
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

abstract class RaffleApp[A <: RaffleArgs, S <: RaffleState, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]] : RunF] {

  protected val implicitRunF: RunF[M] = implicitly[RunF[M]]

  import implicitRunF.{Input, run}


  protected val raffleBehavior: Behavior[A, S, RaffleCommand, RaffleEvent, RaffleId, M]

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

  protected type Output = (S, Unit)

  def runApp() {
    val raffle: M[Unit] = raffleBehavior.handleAll(raffleCommands)
    val output: Output = run(raffle)(input)
    val state: S = output._1
    println("\n================================================================================")
    println(state)
    println("================================================================================\n")
  }

}
