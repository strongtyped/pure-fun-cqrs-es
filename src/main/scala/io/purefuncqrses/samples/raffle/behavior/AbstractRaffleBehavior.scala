package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.{HandlerForAll, History, PartialHandler}
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import shapeless.{HList, HNil}

import scala.collection.immutable
import scala.language.higherKinds
import scala.util.Random

object AbstractRaffleBehavior {
  type RaffleCommands = immutable.Seq[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
  type PartialRaffleCommandHandler[M[+ _]] = PartialHandler[RaffleCommand, M]
  type PartialRaffleCommandHandlers[M[+ _]] = List[PartialRaffleCommandHandler[M]]
  type RaffleCommandHandlerForAll[M[+ _]] = HandlerForAll[RaffleCommand, M]
}

import AbstractRaffleBehavior._

abstract class AbstractRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends Behavior[RaffleCommand, RaffleEvent, RaffleId, M] {

  import implicitFailureF._

  protected def isRaffleCreated(hList: HList): Boolean

  protected def getRaffleId(hList: HList): RaffleId

  protected def participants(hList: HList): Seq[String]

  protected def hasParticipantBeenAdded(name: String, hList: HList): Boolean

  protected def createRaffleCondition(hList: HList): Boolean =
    !isRaffleCreated(hList)

  protected def createRaffleAddingParticipantCondition(hList: HList): Boolean =
    !isRaffleCreated(hList)

  protected def addParticipantCondition(name: String)(hList: HList): Boolean =
    isRaffleCreated(hList) && !hasParticipantBeenAdded(name, hList)

  protected def removeParticipantCondition(name: String)(hList: HList): Boolean =
    isRaffleCreated(hList) && hasParticipantBeenAdded(name, hList)

  protected def selectWinnerCondition(hList: HList): Boolean = {
    isRaffleCreated(hList) && participants(hList).nonEmpty
  }

  protected def commandHandlerBodyTemplate(condition: => HList => Boolean, newState: HList => HList)(command: RaffleCommand, hList: HList): M[Unit] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    if (condition(hList)) {
      setState(newState(hList))
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  protected def commandHandlerWithNameBodyTemplate(condition: => HList => Boolean, newState: HList => HList)(command: RaffleCommandWithName, hList: HList): M[Unit] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    if (condition(hList)) {
      setState(newState(hList))
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  protected def createRaffleCommandHandlerBody(command: RaffleCommand, hList: HList): M[Unit] =
    commandHandlerBodyTemplate(createRaffleCondition, newStateForCreateRaffle)(command, hList)

  protected def createRaffleAddingParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit] =
    commandHandlerWithNameBodyTemplate(createRaffleAddingParticipantCondition, newStateForCreateRaffleAddingParticipant(command.name))(command, hList)

  protected def addParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit] =
    commandHandlerWithNameBodyTemplate(addParticipantCondition(command.name), newStateForAddParticipant(command.name))(command, hList)

  protected def removeParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit] =
    commandHandlerWithNameBodyTemplate(removeParticipantCondition(command.name), newStateForRemoveParticipant(command.name))(command, hList)

  protected def selectWinnerCommandHandlerBody(command: RaffleCommand, hList: HList): M[Unit] =
    commandHandlerBodyTemplate(selectWinnerCondition, newStateForSelectWinner)(command, hList)

  protected def setState(hList: HList): M[Unit]

  protected def raffleCommandHandlerTemplate(command: RaffleCommand, commandHandlerBody: (RaffleCommand, HList) => M[Unit]): M[Unit]

  protected def raffleCommandWithNameHandlerTemplate(command: RaffleCommandWithName, commandWithNameHandlerBody: (RaffleCommandWithName, HList) => M[Unit]): M[Unit]

  protected def newRaffleHistoryForCreateRaffleFrom(hList: HList): (RaffleId, RaffleHistory) = {
    val raffleId = RaffleId.generate()
    val currentRaffleHistory: RaffleHistory = hList._1
    val newRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    println(s"new raffle history = $newRaffleHistory")
    (raffleId, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForCreateRaffleFrom(raffleId: RaffleId): Option[RaffleState] = {
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newStateForCreateRaffle(hList: HList): HList

  protected def newRaffleHistoryForCreateRaffleWithAddingParticipantFrom(name: String, hList: HList): (RaffleId, RaffleHistory) = {
    val raffleId = RaffleId.generate()
    val currentRaffleHistory: RaffleHistory = hList._1
    val tmpRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    val newRaffleHistory = tmpRaffleHistory :+ ParticipantAddedEvent(name, raffleId)
    println(s"new raffle history = $newRaffleHistory")
    (raffleId, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForCreateRaffleWithAddingParticipantFrom(name: String, raffleId: RaffleId): Option[RaffleState] = {
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newStateForCreateRaffleAddingParticipant(name: String)(hList: HList): HList

  protected def newRaffleHistoryForAddParticipantFrom(name: String, hList: HList): RaffleHistory = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(hList))
    println(s"new raffle history = $newRaffleHistory")
    newRaffleHistory
  }

  protected def newOptionalRaffleStateForAddParticipantFrom(name: String, hList: HList) = {
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newStateForAddParticipant(name: String)(hList: HList): HList

  protected def newRaffleHistoryForRemoveParticipantFrom(name: String, hList: HList): RaffleHistory = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId(hList))
    println(s"new raffle history = $newRaffleHistory")
    newRaffleHistory
  }

  protected def newOptionalRaffleStateForRemoveParticipantFrom(name: String, hList: HList) = {
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newStateForRemoveParticipant(name: String)(hList: HList): HList

  protected def newRaffleHistoryForSelectWinnerFrom(hList: HList): (String, RaffleHistory) = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val currentParticipants = participants(hList)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(hList))
    println(s"new raffle history = $newRaffleHistory")
    (winner, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForSelectWinnerFrom(winner: String, hList: HList): Option[RaffleState] = {
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, winner)
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newStateForSelectWinner(hList: HList): HList

  private lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      raffleCommandHandlerTemplate(command, createRaffleCommandHandlerBody)
  }

  private lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(command, createRaffleAddingParticipantCommandHandlerBody)
  }

  private lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(command, addParticipantCommandHandlerBody)
  }

  private lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(command, removeParticipantCommandHandlerBody)
  }

  private lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      raffleCommandHandlerTemplate(command, selectWinnerCommandHandlerBody)
  }

  override protected val partialHandlers: PartialRaffleCommandHandlers[M] =
    List(
      createRaffleCommandHandler,
      createRaffleAddingParticipantCommandHandler,
      addParticipantCommandHandler,
      removeParticipantCommandHandler,
      selectWinnerCommandHandler
    )

}


