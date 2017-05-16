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

import scala.collection.immutable
import scala.language.higherKinds
import scala.util.Random

object AbstractRaffleBehavior {
  type RaffleCommands = immutable.Seq[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
  type PartialRaffleCommandHandler[M[+ _]] = PartialHandler[RaffleCommand, M]
  type HandlerBody[C, M[+ _]] = (C, Args) => M[Unit]
  type PartialRaffleCommandHandlers[M[+ _]] = List[PartialRaffleCommandHandler[M]]
  type RaffleCommandHandlerForEach[M[+ _]] = HandlerForEach[RaffleCommand, M]
}

import AbstractRaffleBehavior._

abstract class AbstractRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[State, ?[_]]]
  extends Behavior[RaffleCommand, RaffleEvent, RaffleId, M] {

  import implicitFailureF._


  protected def isRaffleCreated(args: Args): Boolean

  protected def getRaffleId(args: Args): RaffleId

  protected def participants(args: Args): Seq[String]

  protected def hasParticipantBeenAdded(name: String, args: Args): Boolean


  protected def setState(args: Args): M[Unit]


  protected def newStateForCreateRaffle(args: Args): Args

  protected def newStateForCreateRaffleAddingParticipant(name: String)(args: Args): Args

  protected def newStateForAddParticipant(name: String)(args: Args): Args

  protected def newStateForRemoveParticipant(name: String)(args: Args): Args

  protected def newStateForSelectWinner(args: Args): Args

  protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M]

  private def raffleCommandHandlerTemplate(commandHandlerBody: HandlerBody[RaffleCommand, M]): Handler[RaffleCommand, M] =
    handlerTemplate[RaffleCommand](commandHandlerBody)

  private def raffleCommandWithNameHandlerTemplate(commandWithNameHandlerBody: HandlerBody[RaffleCommandWithName, M]): Handler[RaffleCommandWithName, M] =
    handlerTemplate[RaffleCommandWithName](commandWithNameHandlerBody)


  protected def createRaffleCondition(args: Args): Boolean =
    !isRaffleCreated(args)

  protected def createRaffleAddingParticipantCondition(args: Args): Boolean =
    !isRaffleCreated(args)

  protected def addParticipantCondition(name: String)(args: Args): Boolean =
    isRaffleCreated(args) && !hasParticipantBeenAdded(name, args)

  protected def removeParticipantCondition(name: String)(args: Args): Boolean =
    isRaffleCreated(args) && hasParticipantBeenAdded(name, args)

  protected def selectWinnerCondition(args: Args): Boolean = {
    isRaffleCreated(args) && participants(args).nonEmpty
  }


  protected def commandHandlerBodyTemplate(condition: => Args => Boolean, newState: Args => Args): HandlerBody[RaffleCommand, M] =
    (command, args) => args match {
      case History_Arg(_) | History_And_OptionalState_Args(_, _) =>
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

  protected val createRaffleCommandHandlerBody: HandlerBody[RaffleCommand, M] =
    commandHandlerBodyTemplate(createRaffleCondition, newStateForCreateRaffle)

  protected val createRaffleAddingParticipantCommandHandlerBody: HandlerBody[RaffleCommandWithName, M] =
    (command, args) => commandHandlerBodyTemplate(createRaffleAddingParticipantCondition, newStateForCreateRaffleAddingParticipant(command.name))(command, args)

  protected val addParticipantCommandHandlerBody: HandlerBody[RaffleCommandWithName, M] =
    (command, args) => commandHandlerBodyTemplate(addParticipantCondition(command.name), newStateForAddParticipant(command.name))(command, args)

  protected val removeParticipantCommandHandlerBody: HandlerBody[RaffleCommandWithName, M] =
    (command, args) => commandHandlerBodyTemplate(removeParticipantCondition(command.name), newStateForRemoveParticipant(command.name))(command, args)

  protected val selectWinnerCommandHandlerBody: HandlerBody[RaffleCommand, M] =
    commandHandlerBodyTemplate(selectWinnerCondition, newStateForSelectWinner)


  protected def newRaffleHistoryForCreateRaffleFrom(args: Args): (RaffleId, RaffleHistory) = {
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

  protected def newRaffleHistoryForCreateRaffleAddingParticipantFrom(name: String, args: Args): (RaffleId, RaffleHistory) = {
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

  protected def newRaffleHistoryForAddParticipantFrom(name: String, args: Args): RaffleHistory = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(args))
    println(s"new raffle history = $newRaffleHistory")
    newRaffleHistory
  }

  protected def newOptionalRaffleStateForAddParticipantFrom(name: String, args: Args): Option[OpenState] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newRaffleHistoryForRemoveParticipantFrom(name: String, args: Args): RaffleHistory = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId(args))
    println(s"new raffle history = $newRaffleHistory")
    newRaffleHistory
  }

  protected def newOptionalRaffleStateForRemoveParticipantFrom(name: String, args: Args): Option[OpenState] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newRaffleHistoryForSelectWinnerFrom(args: Args): (String, RaffleHistory) = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val currentParticipants = participants(args)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(args))
    println(s"new raffle history = $newRaffleHistory")
    (winner, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForSelectWinnerFrom(winner: String, args: Args): Option[RaffleState] = {
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


