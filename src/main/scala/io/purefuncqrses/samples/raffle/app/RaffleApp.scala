package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.{All, History, empty, seq}
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{RaffleCommands, RaffleHistory}
import io.purefuncqrses.samples.raffle.behavior.{StatefulRaffleBehavior, StatelessRaffleBehavior}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class RaffleApp[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]] : RunF] {

  private val implicitRunF = implicitly[RunF[M]]

  import implicitRunF._

  //    val raffleBehavior: StatelessRaffleBehavior[M] = new StatelessRaffleBehavior[M]

  val raffleBehavior: StatefulRaffleBehavior[M] = new StatefulRaffleBehavior[M]

  import raffleBehavior._

  val raffleCommands: RaffleCommands =
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

  type Output = (History[RaffleEvent], Unit)

  val raffleHistory: RaffleHistory = {
    val input: Input = (empty, ()).asInstanceOf[Input]
    val output: Output = run(handle(raffleCommands))(input)
    output._1
  }

  println("\n" + raffleHistory.last)

}
