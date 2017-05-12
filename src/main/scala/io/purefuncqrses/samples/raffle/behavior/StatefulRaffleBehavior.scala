package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.{All, History, empty}
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.collection.immutable
import io.purefuncqrses.features.ops.FeatureOps._

import scala.language.higherKinds
import scala.util.Random

object StatefulRaffleBehavior {
  type RaffleCommands = All[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
}

import StatefulRaffleBehavior._

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]]]
  extends Behavior[RaffleCommand, RaffleEvent, RaffleId, M] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  private val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  private val implicitStateF = implicitly[StateF[RaffleHistory, M]]

  import implicitStateF._

  sealed trait RaffleState {
    def raffleId: RaffleId
  }

  case class OpenState(raffleId: RaffleId, participants: List[String] = List())
    extends RaffleState

  case class ClosedState(raffleId: RaffleId, winner: String)
    extends RaffleState

  var optionalRaffleState: Option[RaffleState] = None

  private def isRaffleCreated: Boolean =
    optionalRaffleState.isDefined

  private def getRaffleId: RaffleId =
    optionalRaffleState.get.raffleId

  private def hasParticipantBeenAdded(name: String): Boolean =
    optionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)

  private def participants: Seq[String] =
    optionalRaffleState.get.asInstanceOf[OpenState].participants

  override protected def handle: RaffleCommand => M[Unit] = {
    case command@CreateRaffleCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (!isRaffleCreated) {
          val raffleId = RaffleId.generate()
          optionalRaffleState = Some(OpenState(raffleId, List()))
          setState {
            val newHistory = currentHistory :+ RaffleCreatedEvent(raffleId)
            println(s"new history = $newHistory")
            newHistory
          }
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
    case command@AddParticipantCommand(name) =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (isRaffleCreated && !hasParticipantBeenAdded(name)) {
          optionalRaffleState =
            optionalRaffleState map { raffleState =>
              val openRaffleState = raffleState.asInstanceOf[OpenState]
              openRaffleState.copy(participants = openRaffleState.participants.add(name))
            }
          setState {
            val newHistory = currentHistory :+ ParticipantAddedEvent(name, getRaffleId)
            println(s"new history = $newHistory")
            newHistory
          }
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
    case command@RemoveParticipantCommand(name) =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (isRaffleCreated && hasParticipantBeenAdded(name)) {
          optionalRaffleState =
            optionalRaffleState map { raffleState =>
              val openRaffleState = raffleState.asInstanceOf[OpenState]
              openRaffleState.copy(participants = openRaffleState.participants.remove(name))
            }
          setState {
            val newHistory = currentHistory :+ ParticipantRemovedEvent(name, getRaffleId)
            println(s"new history = $newHistory")
            newHistory
          }
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
    case command@SelectWinnerCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (isRaffleCreated) {
          val currentParticipants = participants
          if (currentParticipants.nonEmpty) {
            val winner = currentParticipants(Random.nextInt(currentParticipants.size))
            optionalRaffleState = optionalRaffleState map { raffleState =>
              ClosedState(raffleState.raffleId, winner)
            }
            setState {
              val newHistory = currentHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId)
              println(s"new history = $newHistory")
              newHistory
            }
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
    case command =>
      println(s"\ncase $command =>")
      failure(new IllegalStateException(s"unknown $command"))
  }

}
