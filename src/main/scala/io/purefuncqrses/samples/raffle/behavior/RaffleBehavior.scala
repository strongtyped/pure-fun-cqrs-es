package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior._
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.collection.immutable
import scala.util.Random

import scala.language.higherKinds

object RaffleBehavior {

  type RaffleCommands = immutable.Seq[RaffleCommand]


  type RaffleHistory = History[RaffleEvent]

  type PartialRaffleCommandHandler[M[+ _]] = PartialHandler[RaffleCommand, M]

  type PartialRaffleCommandHandlers[M[+ _]] = List[PartialRaffleCommandHandler[M]]


  type HandleAllRaffleCommands[M[+ _]] = HandleAll[RaffleCommand, M]

}

import RaffleBehavior._

abstract class RaffleBehavior[A <: RaffleArgs, S <: RaffleState, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]]
  extends Behavior[A, S, RaffleCommand, RaffleEvent, RaffleId, M] {

  //
  // basic functions
  //
  protected def getRaffleId(args: A): RaffleId

  protected def participants(args: A): Seq[String]

  protected def winner(args: A): String = {
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
      println(s"\ncase $command =>")
      handlerTemplate[CreateRaffleCommand.type](createRaffleCondition, newArgsForCreateRaffle)(command)
  }

  private lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      println(s"\ncase $command =>")
      handlerTemplate[CreateRaffleAddingParticipantCommand](createRaffleAddingParticipantCondition, newArgsForCreateRaffleAddingParticipant(command.name))(command)
  }

  private lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      println(s"\ncase $command =>")
      handlerTemplate[AddParticipantCommand](addParticipantCondition(command.name), newArgsForAddParticipant(command.name))(command)
  }

  private lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      println(s"\ncase $command =>")
      handlerTemplate[RemoveParticipantCommand](removeParticipantCondition(command.name), newArgsForRemoveParticipant(command.name))(command)
  }


  private lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      println(s"\ncase $command =>")
      handlerTemplate[SelectWinnerCommand.type](selectWinnerCondition, newArgsForSelectWinner)(command)
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


