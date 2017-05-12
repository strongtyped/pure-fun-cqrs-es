package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.{All, History}
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.collection.immutable
import io.purefuncqrses.features.ops.FeatureOps._

import scala.language.higherKinds
import scala.util.Random

sealed trait RaffleState {
  def raffleId: RaffleId
}

case class OpenState(raffleId: RaffleId, participants: List[String] = List())
  extends RaffleState

case class ClosedState(raffleId: RaffleId, winner: String)
  extends RaffleState

object PureStatefulRaffleBehavior {
  type RaffleCommands = All[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
}

import PureStatefulRaffleBehavior.RaffleHistory

class PureStatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]] : NestedStateF[Option[RaffleState], ?[_]]]
  extends Behavior[RaffleCommand, RaffleEvent, RaffleId, M] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  private val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  private val implicitRaffleHistoryStateF = implicitly[StateF[RaffleHistory, M]]

  import implicitRaffleHistoryStateF._

  private val implicitNestedOptionalRaffleStateStateF = implicitly[NestedStateF[Option[RaffleState], M]]

  import implicitNestedOptionalRaffleStateStateF._

  private def isRaffleCreated(optionalRaffleState: Option[RaffleState]): Boolean =
    optionalRaffleState.isDefined

  private def getRaffleId(optionalRaffleState: Option[RaffleState]): RaffleId =
    optionalRaffleState.get.raffleId

  private def hasParticipantBeenAdded(name: String, optionalRaffleState: Option[RaffleState]): Boolean =
    optionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)

  private def participants(optionalRaffleState: Option[RaffleState]): Seq[String] =
    optionalRaffleState.get.asInstanceOf[OpenState].participants

  override protected def handle: RaffleCommand => M[Unit] = {
    case command@CreateRaffleCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (!isRaffleCreated(currentOptionalState)) {
            val raffleId = RaffleId.generate()
            val newOptionalState = Some(OpenState(raffleId, List()))
            println(s"\nnew optional state = $newOptionalState")
            setNestedState(newOptionalState) flatMap { _ =>
              setState {
                val newHistory = currentHistory :+ RaffleCreatedEvent(raffleId)
                println(s"\nnew history = $newHistory")
                newHistory
              }
            }
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
    case command@AddParticipantCommand(name) =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (isRaffleCreated(currentOptionalState) && !hasParticipantBeenAdded(name, currentOptionalState)) {
            val newOptionalState = currentOptionalState map { state =>
              val openState = state.asInstanceOf[OpenState]
              openState.copy(participants = openState.participants.add(name))
            }
            println(s"\nnew optional state = $newOptionalState")
            setNestedState(newOptionalState) flatMap { _ =>
              setState {
                val newHistory = currentHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalState))
                println(s"\nnew history = $newHistory")
                newHistory
              }
            }
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
    case command@RemoveParticipantCommand(name) =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (isRaffleCreated(currentOptionalState) && hasParticipantBeenAdded(name, currentOptionalState)) {
            val newOptionalState = currentOptionalState map { state =>
              val openRaffleState = state.asInstanceOf[OpenState]
              openRaffleState.copy(participants = openRaffleState.participants.remove(name))
            }
            println(s"\nnew optional state = $newOptionalState")
            setNestedState(newOptionalState) flatMap { _ =>
              setState {
                val newHistory = currentHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentOptionalState))
                println(s"\nnew history = $newHistory")
                newHistory
              }
            }
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
    case command@SelectWinnerCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (isRaffleCreated(currentOptionalState)) {
            val currentParticipants = participants(currentOptionalState)
            if (currentParticipants.nonEmpty) {
              val winner = currentParticipants(Random.nextInt(currentParticipants.size))
              val newOptionalState = currentOptionalState map { state =>
                ClosedState(state.raffleId, winner)
              }
              println(s"\nnew optional state = $newOptionalState")
              setNestedState(newOptionalState) flatMap { _ =>
                setState {
                  val newHistory = currentHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(currentOptionalState))
                  println(s"\nnew history = $newHistory")
                  newHistory
                }
              }
            } else {
              failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
            }
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
    case command =>
      println(s"\ncase $command =>")
      failure(new IllegalStateException(s"unknown $command"))
  }

}
