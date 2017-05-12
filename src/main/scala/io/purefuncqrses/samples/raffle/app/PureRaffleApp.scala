package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.{All, History, all, empty}
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior.PureStatefulRaffleBehavior.RaffleCommands
import io.purefuncqrses.samples.raffle.behavior.{PureStatefulRaffleBehavior, RaffleState}
import io.purefuncqrses.samples.raffle.behavior.StatelessRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class PureRaffleApp[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]] : NestedStateF[Option[RaffleState], ?[_]] : RunF] {

  private val implicitRunF = implicitly[RunF[M]]

  import implicitRunF._

  val raffleBehavior: PureStatefulRaffleBehavior[M] = new PureStatefulRaffleBehavior[M]

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
    (commands: RaffleCommands) => {
      val historyComputation: M[Unit] = raffleBehavior.handleAll(commands)
      val input: Input = (empty, (None, ())).asInstanceOf[Input]
      val (_, (history, _)) = run[Unit, (Option[RaffleState], (History[RaffleEvent], Unit))](historyComputation)(input)
      history
    }

  val raffleHistory: RaffleHistory = runAll(raffleCommands)

  println("\n" + raffleHistory.last)

}
