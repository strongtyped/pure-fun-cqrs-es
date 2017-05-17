package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior._
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.commands.{RaffleCommand, _}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.collection.immutable
import scala.util.Random

import scala.language.higherKinds

object RaffleBehavior {
  type RaffleCommands = immutable.Seq[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
  type PartialRaffleCommandHandler[M[+ _]] = PartialHandler[RaffleCommand, M]
  type HandlerBody[A <: Args, C, M[+ _]] = (C, A) => M[Unit]
  type PartialRaffleCommandHandlers[M[+ _]] = List[PartialRaffleCommandHandler[M]]
  type RaffleCommandHandlerForAll[M[+ _]] = HandlerForAll[RaffleCommand, M]
}

import RaffleBehavior._

abstract class RaffleBehavior[A <: Args, S <: State, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]]
  extends Behavior[A, S, RaffleCommand, RaffleEvent, RaffleId, M] {

  import implicitFailureF._


  protected def isRaffleCreated(args: A): Boolean

  protected def getRaffleId(args: A): RaffleId

  protected def participants(args: A): Seq[String]

  protected def hasParticipantBeenAdded(name: String, args: A): Boolean


  protected def setState(args: A): M[Unit]


  protected def newStateForCreateRaffle(args: A): A

  protected def newStateForCreateRaffleAddingParticipant(name: String)(args: A): A

  protected def newStateForAddParticipant(name: String)(args: A): A

  protected def newStateForRemoveParticipant(name: String)(args: A): A

  protected def newStateForSelectWinner(args: A): A

  protected def handlerTemplate[Cmd](handlerBody: HandlerBody[A, Cmd, M]): Handler[Cmd, M]

  private def raffleCommandHandlerTemplate(commandHandlerBody: HandlerBody[A, RaffleCommand, M]): Handler[RaffleCommand, M] =
    handlerTemplate[RaffleCommand](commandHandlerBody)

  private def raffleCommandWithNameHandlerTemplate(commandWithNameHandlerBody: HandlerBody[A, RaffleCommandWithName, M]): Handler[RaffleCommandWithName, M] =
    handlerTemplate[RaffleCommandWithName](commandWithNameHandlerBody)


  protected def createRaffleCondition(args: A): Boolean =
    !isRaffleCreated(args)

  protected def createRaffleAddingParticipantCondition(args: A): Boolean =
    !isRaffleCreated(args)

  protected def addParticipantCondition(name: String)(args: A): Boolean =
    isRaffleCreated(args) && !hasParticipantBeenAdded(name, args)

  protected def removeParticipantCondition(name: String)(args: A): Boolean =
    isRaffleCreated(args) && hasParticipantBeenAdded(name, args)

  protected def selectWinnerCondition(args: A): Boolean = {
    isRaffleCreated(args) && participants(args).nonEmpty
  }


  protected def commandHandlerBodyTemplate(condition: => A => Boolean, newState: A => A): HandlerBody[A, RaffleCommand, M] =
    (command, args) => args match {
      case HistoryArg(_) | HistoryAndOptionalStateArgs(_, _) =>
        val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (condition(args)) {
          setState(newState(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      case _ =>
        failure(new IllegalStateException(s"no history in $args"))
    }

  protected val createRaffleCommandHandlerBody: HandlerBody[A, RaffleCommand, M] =
    commandHandlerBodyTemplate(createRaffleCondition, newStateForCreateRaffle)

  protected val createRaffleAddingParticipantCommandHandlerBody: HandlerBody[A, RaffleCommandWithName, M] =
    (command, args) => commandHandlerBodyTemplate(createRaffleAddingParticipantCondition, newStateForCreateRaffleAddingParticipant(command.name))(command, args)

  protected val addParticipantCommandHandlerBody: HandlerBody[A, RaffleCommandWithName, M] =
    (command, args) => commandHandlerBodyTemplate(addParticipantCondition(command.name), newStateForAddParticipant(command.name))(command, args)

  protected val removeParticipantCommandHandlerBody: HandlerBody[A, RaffleCommandWithName, M] =
    (command, args) => commandHandlerBodyTemplate(removeParticipantCondition(command.name), newStateForRemoveParticipant(command.name))(command, args)

  protected val selectWinnerCommandHandlerBody: HandlerBody[A, RaffleCommand, M] =
    commandHandlerBodyTemplate(selectWinnerCondition, newStateForSelectWinner)


  protected def newRaffleHistoryForCreateRaffleFrom(args: A): (RaffleId, RaffleHistory) = {
    val raffleId = RaffleId.generate()
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val newRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    println(s"new raffle history = $newRaffleHistory")
    (raffleId, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForCreateRaffleFrom(raffleId: RaffleId): Option[RaffleState] = {
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newRaffleHistoryForCreateRaffleAddingParticipantFrom(name: String, args: A): (RaffleId, RaffleHistory) = {
    val raffleId = RaffleId.generate()
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
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

  protected def newRaffleHistoryForAddParticipantFrom(name: String, args: A): RaffleHistory = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(args))
    println(s"new raffle history = $newRaffleHistory")
    newRaffleHistory
  }

  protected def newOptionalRaffleStateForAddParticipantFrom(name: String, args: A): Option[OpenState] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newRaffleHistoryForRemoveParticipantFrom(name: String, args: A): RaffleHistory = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId(args))
    println(s"new raffle history = $newRaffleHistory")
    newRaffleHistory
  }

  protected def newOptionalRaffleStateForRemoveParticipantFrom(name: String, args: A): Option[OpenState] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newRaffleHistoryForSelectWinnerFrom(args: A): (String, RaffleHistory) = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val currentParticipants = participants(args)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(args))
    println(s"new raffle history = $newRaffleHistory")
    (winner, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForSelectWinnerFrom(winner: String, args: A): Option[RaffleState] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
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


