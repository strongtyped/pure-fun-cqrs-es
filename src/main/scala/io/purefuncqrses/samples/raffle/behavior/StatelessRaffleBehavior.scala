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

  private def createRaffleCondition(currentHistory: RaffleHistory): Boolean =
    !isRaffleCreated(currentHistory)

  private def createRaffleAddingParticipantCondition(currentHistory: RaffleHistory): Boolean =
    !isRaffleCreated(currentHistory)

  private def addParticipantCondition(name: String, currentHistory: RaffleHistory): Boolean =
    isRaffleCreated(currentHistory) && !hasParticipantBeenAdded(name, currentHistory)

  private def removeParticipantCondition(name: String, currentHistory: RaffleHistory): Boolean =
    isRaffleCreated(currentHistory) && hasParticipantBeenAdded(name, currentHistory)

  private def selectWinnerCondition(currentHistory: RaffleHistory): Boolean =
    isRaffleCreated(currentHistory) && participants(currentHistory).nonEmpty

  private def createRaffleBlock(currentHistory: RaffleHistory): M[Unit] =
    setState1 {
      val newHistory = currentHistory :+ RaffleCreatedEvent(RaffleId.generate())
      println(s"new history = $newHistory")
      newHistory
    }

  private def createRaffleAddingParticipantBlock(name: String, currentHistory: RaffleHistory): M[Unit] =
    setState1 {
      val tmpHistory = currentHistory :+ RaffleCreatedEvent(RaffleId.generate())
      val newHistory = tmpHistory :+ ParticipantAddedEvent(name, getRaffleId(tmpHistory))
      println(s"new history = $newHistory")
      newHistory
    }

  private def addParticipantBlock(name: String, currentHistory: RaffleHistory): M[Unit] =
    setState1 {
      val newHistory = currentHistory :+ ParticipantAddedEvent(name, getRaffleId(currentHistory))
      println(s"new history = $newHistory")
      newHistory
    }

  private def removeParticipantBlock(name: String, currentHistory: RaffleHistory): M[Unit] =
    setState1 {
      val newHistory = currentHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentHistory))
      println(s"new history = $newHistory")
      newHistory
    }

  private def selectWinnerBlock(currentHistory: RaffleHistory): M[Unit] = {
    val currentParticipants = participants(currentHistory)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    setState1 {
      val newHistory = currentHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(currentHistory))
      println(s"new history = $newHistory")
      newHistory
    }
  }

  override protected lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command@CreateRaffleCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (createRaffleCondition(currentHistory)) {
          createRaffleBlock(currentHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
  }

  override protected lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (createRaffleAddingParticipantCondition(currentHistory)) {
          createRaffleAddingParticipantBlock(command.name, currentHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
  }

  override protected lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (addParticipantCondition(command.name, currentHistory)) {
          addParticipantBlock(command.name, currentHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
  }

  override protected lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (removeParticipantCondition(command.name, currentHistory)) {
          removeParticipantBlock(command.name, currentHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
  }

  override protected lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (selectWinnerCondition(currentHistory)) {
          selectWinnerBlock(currentHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
  }

}
