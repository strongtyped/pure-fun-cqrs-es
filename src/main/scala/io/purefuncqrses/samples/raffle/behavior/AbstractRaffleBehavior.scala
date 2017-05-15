package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.commands.{RaffleCommand, _}
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
  type HandlerBody[C, M[+ _]] = (C, HList) => M[Unit]
  type PartialRaffleCommandHandlers[M[+ _]] = List[PartialRaffleCommandHandler[M]]
  type RaffleCommandHandlerForEach[M[+ _]] = HandlerForEach[RaffleCommand, M]
}

import AbstractRaffleBehavior._

abstract class AbstractRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends Behavior[RaffleCommand, RaffleEvent, RaffleId, M] {

  import implicitFailureF._


  protected def isRaffleCreated(hList: HList): Boolean

  protected def getRaffleId(hList: HList): RaffleId

  protected def participants(hList: HList): Seq[String]

  protected def hasParticipantBeenAdded(name: String, hList: HList): Boolean


  protected def setState(hList: HList): M[Unit]


  protected def newStateForCreateRaffle(hList: HList): HList

  protected def newStateForCreateRaffleAddingParticipant(name: String)(hList: HList): HList

  protected def newStateForAddParticipant(name: String)(hList: HList): HList

  protected def newStateForRemoveParticipant(name: String)(hList: HList): HList

  protected def newStateForSelectWinner(hList: HList): HList

  protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M]

  private  def raffleCommandHandlerTemplate(commandHandlerBody: HandlerBody[RaffleCommand, M]): Handler[RaffleCommand, M] =
    handlerTemplate[RaffleCommand](commandHandlerBody)

  private def raffleCommandWithNameHandlerTemplate(commandWithNameHandlerBody: HandlerBody[RaffleCommandWithName, M]): Handler[RaffleCommandWithName, M] =
    handlerTemplate[RaffleCommandWithName](commandWithNameHandlerBody)


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


  protected def commandHandlerBodyTemplate(condition: => HList => Boolean, newState: HList => HList): HandlerBody[RaffleCommand, M] =
    (command, hList) => {
    val currentRaffleHistory: RaffleHistory = hList._1
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    if (condition(hList)) {
      setState(newState(hList))
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  protected val createRaffleCommandHandlerBody: HandlerBody[RaffleCommand, M] =
    commandHandlerBodyTemplate(createRaffleCondition, newStateForCreateRaffle)

  protected val createRaffleAddingParticipantCommandHandlerBody: HandlerBody[RaffleCommandWithName, M] =
    (command, hList) => commandHandlerBodyTemplate(createRaffleAddingParticipantCondition, newStateForCreateRaffleAddingParticipant(command.name))(command, hList)

  protected val addParticipantCommandHandlerBody: HandlerBody[RaffleCommandWithName, M] =
    (command, hList) => commandHandlerBodyTemplate(addParticipantCondition(command.name), newStateForAddParticipant(command.name))(command, hList)

  protected val removeParticipantCommandHandlerBody: HandlerBody[RaffleCommandWithName, M] =
    (command, hList) => commandHandlerBodyTemplate(removeParticipantCondition(command.name), newStateForRemoveParticipant(command.name))(command, hList)

  protected val selectWinnerCommandHandlerBody: HandlerBody[RaffleCommand, M] =
    commandHandlerBodyTemplate(selectWinnerCondition, newStateForSelectWinner)


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

  protected def newRaffleHistoryForCreateRaffleAddingParticipantFrom(name: String, hList: HList): (RaffleId, RaffleHistory) = {
    val raffleId = RaffleId.generate()
    val currentRaffleHistory: RaffleHistory = hList._1
    val tmpRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    val newRaffleHistory = tmpRaffleHistory :+ ParticipantAddedEvent(name, raffleId)
    println(s"new raffle history = $newRaffleHistory")
    (raffleId, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForCreateRaffleAddingParticipantFrom(name: String, raffleId: RaffleId): Option[RaffleState] = {
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

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

  private lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      raffleCommandHandlerTemplate(createRaffleCommandHandlerBody)(command)
  }

  private lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(createRaffleAddingParticipantCommandHandlerBody)(command)
  }

  private lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(addParticipantCommandHandlerBody)(command)
  }

  private lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      raffleCommandWithNameHandlerTemplate(removeParticipantCommandHandlerBody)(command)
  }

  private lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      raffleCommandHandlerTemplate(selectWinnerCommandHandlerBody)(command)
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


