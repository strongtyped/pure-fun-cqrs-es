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

  private def createRaffleBlock(currentHistory: RaffleHistory): M[Unit] = {
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    val raffleId = RaffleId.generate()
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      val newHistory = currentHistory :+ RaffleCreatedEvent(raffleId)
      println(s"new history = $newHistory")
      newHistory
    }
  }

  private def createRaffleAddingParticipantBlock(name: String, currentHistory: RaffleHistory): M[Unit] = {
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    val raffleId = RaffleId.generate()
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      val tmpHistory = currentHistory :+ RaffleCreatedEvent(raffleId)
      val newHistory = tmpHistory :+ ParticipantAddedEvent(name, getRaffleId)
      println(s"new history = $newHistory")
      newHistory
    }
  }

  private def addParticipantBlock(name: String, currentHistory: RaffleHistory): M[Unit] = {
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openRaffleState = currentRaffleState.asInstanceOf[OpenState]
      openRaffleState.copy(participants = openRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      val newHistory = currentHistory :+ ParticipantAddedEvent(name, getRaffleId)
      println(s"new history = $newHistory")
      newHistory
    }
  }

  private def removeParticipantBlock(name: String, currentHistory: RaffleHistory): M[Unit] = {
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openRaffleState = currentRaffleState.asInstanceOf[OpenState]
      openRaffleState.copy(participants = openRaffleState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      val newHistory = currentHistory :+ ParticipantRemovedEvent(name, getRaffleId)
      println(s"new history = $newHistory")
      newHistory
    }
  }

  private def selectWinnerBlock(currentHistory: RaffleHistory): M[Unit] = {
    val currentParticipants = participants
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, winner)
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    currentOptionalRaffleState = newOptionalRaffleState
    setState1 {
      val newHistory = currentHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId)
      println(s"new history = $newHistory")
      newHistory
    }
  }

  override protected lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        if (createRaffleCondition) {
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
        if (createRaffleAddingParticipantCondition) {
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
        if (addParticipantCondition(command.name)) {
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
        if (removeParticipantCondition(command.name)) {
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
        if (selectWinnerCondition) {
          selectWinnerBlock(currentHistory)
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
      }
  }

}
