package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{PartialRaffleCommandHandler, RaffleHistory}

import scala.language.higherKinds
import scala.util.Random

class StatelessRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  private val implicitStateF = implicitly[State1F[RaffleHistory, M]]

  import implicitStateF._

  private def isRaffleCreated(currentRaffleHistory: RaffleHistory): Boolean =
    currentRaffleHistory.nonEmpty

  private def getRaffleId(currentRaffleHistory: RaffleHistory): RaffleId =
    currentRaffleHistory.head.asInstanceOf[RaffleCreatedEvent].raffleId

  private def hasParticipantBeenAdded(name: String, currentRaffleHistory: RaffleHistory): Boolean = {
    val numberOfTimesAdded =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantAddedEvent] && raffleEvent.asInstanceOf[ParticipantAddedEvent].name == name)
    val numberOfTimesRemoved =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantRemovedEvent] && raffleEvent.asInstanceOf[ParticipantRemovedEvent].name == name)
    numberOfTimesAdded > numberOfTimesRemoved
  }

  private def participants(currentRaffleHistory: RaffleHistory): Seq[String] = {
    currentRaffleHistory.tail.foldLeft(List[String]()) { (participants, raffleEvent) =>
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

  private def createRaffleCondition(currentRaffleHistory: RaffleHistory): Boolean =
    !isRaffleCreated(currentRaffleHistory)

  private def createRaffleAddingParticipantCondition(currentRaffleHistory: RaffleHistory): Boolean =
    !isRaffleCreated(currentRaffleHistory)

  private def addParticipantCondition(name: String, currentRaffleHistory: RaffleHistory): Boolean =
    isRaffleCreated(currentRaffleHistory) && !hasParticipantBeenAdded(name, currentRaffleHistory)

  private def removeParticipantCondition(name: String, currentRaffleHistory: RaffleHistory): Boolean =
    isRaffleCreated(currentRaffleHistory) && hasParticipantBeenAdded(name, currentRaffleHistory)

  private def selectWinnerCondition(currentRaffleHistory: RaffleHistory): Boolean =
    isRaffleCreated(currentRaffleHistory) && participants(currentRaffleHistory).nonEmpty

  private def createRaffleBlock(currentRaffleHistory: RaffleHistory): M[Unit] = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    println(s"new raffle history = $newRaffleHistory")
    setState1 {
      newRaffleHistory
    }
  }

  private def createRaffleAddingParticipantBlock(name: String, currentRaffleHistory: RaffleHistory): M[Unit] = {
    val tmpRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(RaffleId.generate())
    val newRaffleHistory = tmpRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(tmpRaffleHistory))
    println(s"new raffle history = $newRaffleHistory")
    setState1 {
      newRaffleHistory
    }
  }

  private def addParticipantBlock(name: String, currentRaffleHistory: RaffleHistory): M[Unit] = {
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(currentRaffleHistory))
    println(s"new raffle history = $newRaffleHistory")
    setState1 {
      newRaffleHistory
    }
  }

  private def removeParticipantBlock(name: String, currentRaffleHistory: RaffleHistory): M[Unit] = {
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentRaffleHistory))
    println(s"new raffle history = $newRaffleHistory")
    setState1 {
      newRaffleHistory
    }
  }

  private def selectWinnerBlock(currentRaffleHistory: RaffleHistory): M[Unit] = {
    val currentParticipants = participants(currentRaffleHistory)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(currentRaffleHistory))
    println(s"new raffle history = $newRaffleHistory")
    setState1 {
      newRaffleHistory
    }
  }

  override protected lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command@CreateRaffleCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (createRaffleCondition(currentRaffleHistory)) {
          createRaffleBlock(currentRaffleHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      }
  }

  override protected lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (createRaffleAddingParticipantCondition(currentRaffleHistory)) {
          createRaffleAddingParticipantBlock(command.name, currentRaffleHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      }
  }

  override protected lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (addParticipantCondition(command.name, currentRaffleHistory)) {
          addParticipantBlock(command.name, currentRaffleHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      }
  }

  override protected lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (removeParticipantCondition(command.name, currentRaffleHistory)) {
          removeParticipantBlock(command.name, currentRaffleHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      }
  }

  override protected lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (selectWinnerCondition(currentRaffleHistory)) {
          selectWinnerBlock(currentRaffleHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      }
  }

}
