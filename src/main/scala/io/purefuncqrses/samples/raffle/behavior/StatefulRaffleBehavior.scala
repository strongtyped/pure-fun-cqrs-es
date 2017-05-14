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

class StatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  private val implicitStateF = implicitly[State1F[RaffleHistory, M]]

  import implicitStateF._

  var currentOptionalRaffleState: Option[RaffleState] = None

  private def isRaffleCreated: Boolean =
    currentOptionalRaffleState.isDefined

  private def getRaffleId: RaffleId =
    currentOptionalRaffleState.get.raffleId

  private def hasParticipantBeenAdded(name: String): Boolean =
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)

  private def participants: Seq[String] =
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants

  private def createRaffleCondition: Boolean =
    !isRaffleCreated

  private def createRaffleAddingParticipantCondition: Boolean =
    !isRaffleCreated

  private def addParticipantCondition(name: String): Boolean =
    isRaffleCreated && !hasParticipantBeenAdded(name)

  private def removeParticipantCondition(name: String): Boolean =
    isRaffleCreated && hasParticipantBeenAdded(name)

  private def selectWinnerCondition: Boolean =
    isRaffleCreated && participants.nonEmpty

  private def createRaffleBlock(currentRaffleHistory: RaffleHistory): M[Unit] = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    println(s"new raffle history = $newRaffleHistory")
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  private def createRaffleAddingParticipantBlock(name: String, currentRaffleHistory: RaffleHistory): M[Unit] = {
    val raffleId = RaffleId.generate()
    val tmpRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    val newRaffleHistory = tmpRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId)
    println(s"new raffle history = $newRaffleHistory")
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  private def addParticipantBlock(name: String, currentRaffleHistory: RaffleHistory): M[Unit] = {
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId)
    println(s"new raffle history = $newRaffleHistory")
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openRaffleState = currentRaffleState.asInstanceOf[OpenState]
      openRaffleState.copy(participants = openRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  private def removeParticipantBlock(name: String, currentRaffleHistory: RaffleHistory): M[Unit] = {
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId)
    println(s"new raffle history = $newRaffleHistory")
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openRaffleState = currentRaffleState.asInstanceOf[OpenState]
      openRaffleState.copy(participants = openRaffleState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  private def selectWinnerBlock(currentRaffleHistory: RaffleHistory): M[Unit] = {
    val currentParticipants = participants
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId)
    println(s"new raffle history = $newRaffleHistory")
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, winner)
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      newRaffleHistory
    }
  }

  override protected lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
        if (createRaffleCondition) {
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
        println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
        if (createRaffleAddingParticipantCondition) {
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
        println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
        if (addParticipantCondition(command.name)) {
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
        println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
        if (removeParticipantCondition(command.name)) {
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
        println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
        if (selectWinnerCondition) {
          selectWinnerBlock(currentRaffleHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      }
  }

}
