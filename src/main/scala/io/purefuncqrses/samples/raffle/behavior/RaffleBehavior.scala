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
import io.purefuncqrses.features.ops.FeatureOps._

import scala.collection.immutable
import scala.util.Random

import scala.language.higherKinds

object RaffleBehavior {
  type RaffleCommands = immutable.Seq[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
  type PartialRaffleCommandHandler[M[+ _]] = PartialHandler[RaffleCommand, M]
  type PartialRaffleCommandHandlers[M[+ _]] = List[PartialRaffleCommandHandler[M]]
  type RaffleCommandHandlerForAll[M[+ _]] = HandlerForAll[RaffleCommand, M]
}

import RaffleBehavior._

abstract class RaffleBehavior[A <: Args, S <: State, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]]
  extends Behavior[A, S, RaffleCommand, RaffleEvent, RaffleId, M] {

  import implicitFailureF._

  import implicitStateF._

  //
  // basic functions
  //
  protected def getRaffleId(args: A): RaffleId

  protected def participants(args: A): Seq[String]

  private def winner(args: A): String = {
    val currentParticipants = participants(args)
    currentParticipants(Random.nextInt(currentParticipants.size))
  }


  //
  // basic predicates
  //
  protected def isRaffleCreated(args: A): Boolean

  protected def hasParticipantBeenAdded(name: String, args: A): Boolean

  private def hasParticipants(args: A): Boolean =
    participants(args).nonEmpty


  //
  // derived predicates
  //
  protected def createRaffleCondition(args: A): Boolean =
    !isRaffleCreated(args)

  protected def createRaffleAddingParticipantCondition(args: A): Boolean =
  !isRaffleCreated(args)

  protected def addParticipantCondition(name: String)(args: A): Boolean =
    isRaffleCreated(args) && !hasParticipantBeenAdded(name, args)

  protected def removeParticipantCondition(name: String)(args: A): Boolean =
    isRaffleCreated(args) && hasParticipantBeenAdded(name, args)

  protected def selectWinnerCondition(args: A): Boolean = {
    isRaffleCreated(args) && hasParticipants(args)
  }


  //
  // derived simple functions
  //
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
    val raffleWinner = winner(args)
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(raffleWinner, OffsetDateTime.now, getRaffleId(args))
    println(s"new raffle history = $newRaffleHistory")
    (raffleWinner, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForSelectWinnerFrom(winner: String, args: A): Option[RaffleState] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, winner)
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  //
  // more complex functions
  //
  protected def newArgsForCreateRaffle(args: A): A

  protected def newArgsForCreateRaffleAddingParticipant(name: String)(args: A): A

  protected def newArgsForAddParticipant(name: String)(args: A): A

  protected def newArgsForRemoveParticipant(name: String)(args: A): A

  protected def newArgsForSelectWinner(args: A): A


  //
  // handlers
  //
  private lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      handlerTemplate[RaffleCommand] { (command, args) =>
        val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (createRaffleCondition(args)) {
          setState(newArgsForCreateRaffle(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      } (command)
  }

  private lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      handlerTemplate[RaffleCommandWithName] { (command, args) =>
        val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (createRaffleAddingParticipantCondition(args)) {
          setState(newArgsForCreateRaffleAddingParticipant(command.name)(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      } (command)
  }

  private lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      handlerTemplate[RaffleCommandWithName] { (command, args) =>
        val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (addParticipantCondition(command.name)(args)) {
          setState(newArgsForAddParticipant(command.name)(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      } (command)
  }

  private lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      handlerTemplate[RaffleCommandWithName] { (command, args) =>
        val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (removeParticipantCondition(command.name)(args)) {
          setState(newArgsForRemoveParticipant(command.name)(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      } (command)
  }


  private lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      handlerTemplate[RaffleCommand] { (command, args) =>
        val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
        println(s"\ncurrent raffle history = $currentRaffleHistory")
        if (selectWinnerCondition(args)) {
          setState(newArgsForSelectWinner(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
        }
      } (command)
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


