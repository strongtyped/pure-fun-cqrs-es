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

object StatelessRaffleBehavior {
  type RaffleCommands = All[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
}

import StatelessRaffleBehavior._

class StatelessRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]]]
  extends Behavior[RaffleCommand, RaffleEvent, RaffleId, M] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  private val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  private val implicitStateF = implicitly[StateF[RaffleHistory, M]]

  import implicitStateF._

  //  private val implicitRunF = implicitly[RunF[M]]
  //
  //  import implicitRunF._

  private def isRaffleCreated(currentHistory: RaffleHistory): Boolean =
    currentHistory.nonEmpty

  private def getRaffleId(currentHistory: RaffleHistory): RaffleId =
    currentHistory.head.asInstanceOf[RaffleCreatedEvent].raffleId

  private def hasParticipantBeenAdded(name: String, currentHistory: RaffleHistory): Boolean = {
    val numberOfTimesAdded =
      currentHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantAddedEvent] && raffleEvent.asInstanceOf[ParticipantAddedEvent].name == name)
    val numberOfTimesRemoved =
      currentHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantRemovedEvent] && raffleEvent.asInstanceOf[ParticipantRemovedEvent].name == name)
    numberOfTimesAdded > numberOfTimesRemoved
  }

  private def participants(currentHistory: RaffleHistory): Seq[String] = {
    currentHistory.tail.foldLeft(List[String]()) { (participants, raffleEvent) =>
      raffleEvent match {
        case ParticipantAddedEvent(name, _) =>
          participants.add(name)
        case ParticipantRemovedEvent(name, _) =>
          participants.remove(name)
        case _ =>
          participants
      }
    }
  }

  override protected def handle: RaffleCommand => M[Unit] = {
    case command@CreateRaffleCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (!isRaffleCreated(currentHistory)) {
          setState {
            val newHistory = currentHistory :+ RaffleCreatedEvent(RaffleId.generate())
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
        if (isRaffleCreated(currentHistory) && !hasParticipantBeenAdded(name, currentHistory)) {
          setState {
            val newHistory = currentHistory :+ ParticipantAddedEvent(name, getRaffleId(currentHistory))
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
        if (isRaffleCreated(currentHistory) && hasParticipantBeenAdded(name, currentHistory)) {
          setState {
            val newHistory = currentHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentHistory))
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
        if (isRaffleCreated(currentHistory)) {
          val currentParticipants = participants(currentHistory)
          if (currentParticipants.nonEmpty) {
            setState {
              val newHistory = currentHistory :+ WinnerSelectedEvent(currentParticipants(Random.nextInt(currentParticipants.size)), OffsetDateTime.now, getRaffleId(currentHistory))
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

  //  override def runAll: RaffleCommands => RaffleHistory =
  //    commands => {
  //      val historyDescription: M[History[Unit]] = handleAll(commands)
  //      val input: Input = (empty, ()).asInstanceOf[Input]
  //      val (history, _) = run[History[Unit], (History[RaffleEvent], History[Unit])](historyDescription)(input)
  //      history
  //    }

}
