package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.util.Util._
import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.{HandlerForAll, History, PartialHandler}
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events.{ParticipantAddedEvent, RaffleCreatedEvent, RaffleEvent}
import io.purefuncqrses.samples.raffle.id.RaffleId
import shapeless.HList

import scala.collection.immutable
import scala.language.higherKinds

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

  protected def createRaffleCommandHandlerBody(command: RaffleCommand, hList: HList): M[Unit]

  protected def createRaffleAddingParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit]

  protected def addParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit]

  protected def removeParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit]

  protected def selectWinnerCommandHandlerBody(command: RaffleCommand, hList: HList): M[Unit]

  protected def setState(hList: HList): M[Unit]

  protected def raffleCommandHandlerTemplate(command: RaffleCommand, commandHandlerBody: (RaffleCommand, HList) => M[Unit]): M[Unit]

  protected def raffleCommandWithNameHandlerTemplate(command: RaffleCommandWithName, commandWithNameHandlerBody: (RaffleCommandWithName, HList) => M[Unit]): M[Unit]

  protected def newRaffleHistoryForCreateRaffleFrom(currentRaffleHistory: RaffleHistory): (RaffleId, RaffleHistory) = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    println(s"new raffle history = $newRaffleHistory")
    (raffleId, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForCreateRaffleFrom(raffleId: RaffleId): Option[RaffleState] = {
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

  protected def newRaffleHistoryForCreateRaffleWithAddingParticipantFrom(currentRaffleHistory: RaffleHistory, name: String): (RaffleId, RaffleHistory) = {
    val raffleId = RaffleId.generate()
    val tmpRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    val newRaffleHistory = tmpRaffleHistory :+ ParticipantAddedEvent(name, raffleId)
    println(s"new raffle history = $newRaffleHistory")
    (raffleId, newRaffleHistory)
  }

  protected def newOptionalRaffleStateForCreateRaffleWithAddingParticipantFrom(raffleId: RaffleId, name: String): Option[RaffleState] = {
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    newOptionalRaffleState
  }

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


