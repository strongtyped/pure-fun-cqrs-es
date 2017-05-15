package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{PartialRaffleCommandHandler, RaffleHistory}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events.{ParticipantAddedEvent, ParticipantRemovedEvent, RaffleCreatedEvent, WinnerSelectedEvent}
import io.purefuncqrses.samples.raffle.id.RaffleId
import shapeless.{HList, HNil}

import scala.util.Random
import scala.language.higherKinds

abstract class AbstractOptimizedRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  protected val implicitRaffleHistoryState1F = implicitly[State1F[RaffleHistory, M]]

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

  private def createRaffleBlock(currentRaffleHistory: RaffleHistory): M[Unit] = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(currentRaffleHistory)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleFrom(raffleId)
    setState(newOptionalRaffleState :: newRaffleHistory :: HNil)
  }

  private def createRaffleAddingParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleWithAddingParticipantFrom(currentRaffleHistory, name)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleWithAddingParticipantFrom(raffleId: RaffleId, name: String)
    setState(newRaffleHistory :: newOptionalRaffleState :: HNil)
  }

  private def addParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    setState(newOptionalRaffleState :: newRaffleHistory :: HNil)
  }

  private def removeParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    setState(newOptionalRaffleState :: newRaffleHistory :: HNil)
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
    setState(newOptionalRaffleState :: newRaffleHistory :: HNil)
  }

  override protected def createRaffleCommandHandlerBody(command: RaffleCommand, hList: HList): M[Unit] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (createRaffleCondition(currentOptionalRaffleState)) {
      createRaffleBlock(currentRaffleHistory)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  override protected def createRaffleAddingParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (createRaffleAddingParticipantCondition(currentOptionalRaffleState)) {
      createRaffleAddingParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  override protected def addParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (addParticipantCondition(command.name, currentOptionalRaffleState)) {
      addParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

  override protected def removeParticipantCommandHandlerBody(command: RaffleCommandWithName, hList: HList): M[Unit] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
      println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
      if (removeParticipantCondition(command.name, currentOptionalRaffleState)) {
        removeParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
      } else {
        failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
      }
  }

  override protected def selectWinnerCommandHandlerBody(command: RaffleCommand, hList: HList): M[Unit] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val currentOptionalRaffleState: Option[RaffleState] = hList._2
    println(s"\ncurrent raffle history = $currentRaffleHistory")
    println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
    if (selectWinnerCondition(currentOptionalRaffleState)) {
      selectWinnerBlock(currentRaffleHistory, currentOptionalRaffleState)
    } else {
      failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
    }
  }

}
