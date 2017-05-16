package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.State
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleCommands
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.behavior.Behavior.seq
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

abstract class AbstractRaffleApp[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]] : RunF] {

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

  protected def input: Input

  protected type Output = (State, Unit)

  protected def output: Output = {
    val raffle: M[Unit] = raffleBehavior.handleAll(raffleCommands)
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
