package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.{All, History, empty, seq}
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{RaffleCommands, RaffleHistory}
import io.purefuncqrses.samples.raffle.behavior.{PureStatefulRaffleBehavior, RaffleState}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class PureRaffleApp[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]] : NestedStateF[Option[RaffleState], ?[_]] : RunF] {

  private val implicitRunF = implicitly[RunF[M]]

  import implicitRunF._

  val raffleBehavior: PureStatefulRaffleBehavior[M] = new PureStatefulRaffleBehavior[M]

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

  val raffle: M[Unit] = handle(raffleCommands)

  type Output = (Option[RaffleState], (History[RaffleEvent], Unit))

  val raffleHistory: RaffleHistory = {
    val input: Input = (empty, (None, ())).asInstanceOf[Input]
    val output: Output = run(raffle)(input)
    output._2._1
  }

  println("\n" + raffleHistory.last)

}
