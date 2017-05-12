package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.{All, History, all, empty}
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.{StatefulRaffleBehavior, StatelessRaffleBehavior}
import io.purefuncqrses.samples.raffle.behavior.StatelessRaffleBehavior.{RaffleCommands, RaffleHistory}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class RaffleApp[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]] : RunF] {

  private val implicitRunF = implicitly[RunF[M]]

  import implicitRunF._

  val raffleBehavior: StatelessRaffleBehavior[M] = new StatelessRaffleBehavior[M]
  // val raffleBehavior: StatefulRaffleBehavior[M] = new StatefulRaffleBehavior[M]

  val raffleCommands: RaffleCommands =
    all(
      CreateRaffleCommand,
      AddParticipantCommand("John"),
      AddParticipantCommand("Paul"),
      AddParticipantCommand("George"),
      AddParticipantCommand("Ringo"),
      RemoveParticipantCommand("Paul"),
      SelectWinnerCommand
    )

  def runAll: RaffleCommands => RaffleHistory =
    commands => {
      val historyDescription: M[History[Unit]] = raffleBehavior.handleAll(commands)
      val input: Input = (empty, ()).asInstanceOf[Input]
      val (history, _) = run[History[Unit], (History[RaffleEvent], History[Unit])](historyDescription)(input)
      history
    }

  val raffleHistory: RaffleHistory = runAll(raffleCommands)

  println("\n" + raffleHistory.last)

}
