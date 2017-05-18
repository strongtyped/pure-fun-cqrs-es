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

  type RaffleHistoryCommandHandlerBlock[M[+ _]] = HandlerBlock[RaffleHistoryArg, M]

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

  //
  // basic derived functions
  //

  protected def winner(args: A): String = {
    val currentParticipants = participants(args)
    currentParticipants(Random.nextInt(currentParticipants.size))
  }


  //
  // basic predicates
  //

  protected def isRaffleCreated(args: A): Boolean

  protected def hasParticipantBeenAdded(name: String, args: A): Boolean

  //
  // basic derived predicates
  //

  private def hasParticipants(args: A): Boolean =
  participants(args).nonEmpty


  //
  // derived conditions
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
  // blocks
  //

  protected def createRaffleBlock: A => M[Unit]

  protected def createRaffleAddingParticipantBlock(name: String): A => M[Unit]

  protected def addParticipantBlock(name: String): A => M[Unit]

  protected def removeParticipantBlock(name: String): A => M[Unit]

  protected def selectWinnerBlock: A => M[Unit]


  //
  // handlers
  //

  private lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      println(s"\ncase $command =>")
      handlerTemplate[CreateRaffleCommand.type](createRaffleCondition, createRaffleBlock)(command)
  }

  private lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      println(s"\ncase $command =>")
      handlerTemplate[CreateRaffleAddingParticipantCommand](createRaffleAddingParticipantCondition, createRaffleAddingParticipantBlock(command.name))(command)
  }

  private lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      println(s"\ncase $command =>")
      handlerTemplate[AddParticipantCommand](addParticipantCondition(command.name), addParticipantBlock(command.name))(command)
  }

  private lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      println(s"\ncase $command =>")
      handlerTemplate[RemoveParticipantCommand](removeParticipantCondition(command.name), removeParticipantBlock(command.name))(command)
  }


  private lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      println(s"\ncase $command =>")
      handlerTemplate[SelectWinnerCommand.type](selectWinnerCondition, selectWinnerBlock)(command)
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


