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

  //  private val implicitNestedRunF = implicitly[NestedRunF[M]]
  //
  //  import implicitNestedRunF._

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
      val historyDescription: M[History[Unit]] = raffleBehavior.handleAll(commands)
      val input: Input = (empty, (None, ())).asInstanceOf[Input]
      val (_, (history, _)) = run[History[Unit], (Option[RaffleState], (History[RaffleEvent], History[Unit]))](historyDescription)(input)
      history
    }

  val raffleHistory: RaffleHistory = runAll(raffleCommands)

  println("\n" + raffleHistory.last)

}
