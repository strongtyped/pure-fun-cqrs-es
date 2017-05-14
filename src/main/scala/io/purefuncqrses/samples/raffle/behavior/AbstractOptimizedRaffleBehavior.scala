package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{PartialRaffleCommandHandler, RaffleHistory}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events.{ParticipantAddedEvent, ParticipantRemovedEvent, RaffleCreatedEvent, WinnerSelectedEvent}
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.util.Random
import scala.language.higherKinds

abstract class AbstractOptimizedRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  protected val implicitRaffleHistoryState1F = implicitly[State1F[RaffleHistory, M]]

  // import implicitRaffleHistoryState1F._

  private def isRaffleCreated(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    currentOptionalRaffleState.isDefined

  private def getRaffleId(currentOptionalRaffleState: Option[RaffleState]): RaffleId =
    currentOptionalRaffleState.get.raffleId

  private def hasParticipantBeenAdded(name: String, currentOptionalRaffleState: Option[RaffleState]): Boolean =
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)

  private def participants(currentOptionalRaffleState: Option[RaffleState]): Seq[String] =
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants

  private def createRaffleCondition(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    !isRaffleCreated(currentOptionalRaffleState)

  private def createRaffleAddingParticipantCondition(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    !isRaffleCreated(currentOptionalRaffleState)

  private def addParticipantCondition(name: String, currentOptionalRaffleState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalRaffleState) && !hasParticipantBeenAdded(name, currentOptionalRaffleState)

  private def removeParticipantCondition(name: String, currentOptionalRaffleState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalRaffleState) && hasParticipantBeenAdded(name, currentOptionalRaffleState)

  private def selectWinnerCondition(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalRaffleState) && participants(currentOptionalRaffleState).nonEmpty

  protected def setState(newOptionalRaffleState: Option[RaffleState], newRaffleHistory: RaffleHistory): M[Unit]

  private def createRaffleBlock(currentRaffleHistory: RaffleHistory): M[Unit] = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    println(s"\nnew raffle history = $newRaffleHistory")
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    setState(newOptionalRaffleState, newRaffleHistory)
  }

  private def createRaffleAddingParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val raffleId = RaffleId.generate()
    val tmpRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    val newRaffleHistory = tmpRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    setState(newOptionalRaffleState, newRaffleHistory)
  }

  private def addParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    setState(newOptionalRaffleState, newRaffleHistory)
  }

  private def removeParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    setState(newOptionalRaffleState, newRaffleHistory)
  }

  private def selectWinnerBlock(currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]): M[Unit] = {
    val currentParticipants = participants(currentOptionalRaffleState)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, winner)
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    setState(newOptionalRaffleState, newRaffleHistory)
  }

  private def createRaffleCommandHandlerBody(command: RaffleCommand, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]): M[Unit] = {
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (createRaffleCondition(currentOptionalRaffleState)) {
      createRaffleBlock(currentRaffleHistory)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  private def createRaffleAddingParticipantCommandHandlerBody(command: RaffleCommandWithName, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]): M[Unit] = {
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (createRaffleAddingParticipantCondition(currentOptionalRaffleState)) {
      createRaffleAddingParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  private def addParticipantCommandHandlerBody(command: RaffleCommandWithName, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]): M[Unit] = {
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (addParticipantCondition(command.name, currentOptionalRaffleState)) {
      addParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  private def removeParticipantCommandHandlerBody(command: RaffleCommandWithName, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]): M[Unit] = {
      println(s"\ncurrent raffle history = $currentRaffleHistory")
      println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
      if (removeParticipantCondition(command.name, currentOptionalRaffleState)) {
        removeParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
      } else {
        failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
      }
  }

  private def selectWinnerCommandHandlerBody(command: RaffleCommand, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]): M[Unit] = {
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (selectWinnerCondition(currentOptionalRaffleState)) {
      selectWinnerBlock(currentRaffleHistory, currentOptionalRaffleState)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  protected def raffleCommandHandlerTemplate(command: RaffleCommand, commandHandlerBody: (RaffleCommand, RaffleHistory, Option[RaffleState]) => M[Unit]): M[Unit]

  protected def raffleCommandWithNameHandlerTemplate(command: RaffleCommandWithName, commandWithNameHandlerBody: (RaffleCommandWithName, RaffleHistory, Option[RaffleState]) => M[Unit]): M[Unit]

  override protected lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      raffleCommandHandlerTemplate(command, createRaffleCommandHandlerBody)
  }

  override protected lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(command, createRaffleAddingParticipantCommandHandlerBody)
  }

  override protected lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(command, addParticipantCommandHandlerBody)
  }

  override protected lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(command, removeParticipantCommandHandlerBody)
  }

  override protected lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      raffleCommandHandlerTemplate(command, selectWinnerCommandHandlerBody)
  }

}
