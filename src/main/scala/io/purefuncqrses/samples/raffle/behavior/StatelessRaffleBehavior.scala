package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import shapeless.{HList, HNil, ::}

import scala.language.higherKinds

class StatelessRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  private val implicitRaffleHistoryState1F = implicitly[State1F[RaffleHistory, M]]

  import implicitRaffleHistoryState1F._

  override protected def isRaffleCreated(hList: HList): Boolean = {
    val currentRaffleHistory: RaffleHistory = hList._1
    currentRaffleHistory.nonEmpty
  }

  override protected def getRaffleId(hList: HList): RaffleId = {
    val currentRaffleHistory: RaffleHistory = hList._1
    currentRaffleHistory.head.asInstanceOf[RaffleCreatedEvent].raffleId
  }

  override protected def participants(hList: HList): Seq[String] = {
    val currentRaffleHistory: RaffleHistory = hList._1
    currentRaffleHistory.tail.foldLeft(List[String]()) { (participants, raffleEvent) =>
      raffleEvent match {
        case ParticipantAddedEvent(name, _) =>
          participants.add(name)
        case ParticipantRemovedEvent(name, _) =>
          participants.remove(name)
        case _ =>
          participants
      }
    }
  }

  override protected def hasParticipantBeenAdded(name: String, hList: HList): Boolean = {
    val currentRaffleHistory: RaffleHistory = hList._1
    val numberOfTimesAdded =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantAddedEvent] && raffleEvent.asInstanceOf[ParticipantAddedEvent].name == name)
    val numberOfTimesRemoved =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantRemovedEvent] && raffleEvent.asInstanceOf[ParticipantRemovedEvent].name == name)
    numberOfTimesAdded > numberOfTimesRemoved
  }

  override protected def newStateForCreateRaffle(hList: HList): HList = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(hList)
    newRaffleHistory :: HNil
  }

  override protected def newStateForCreateRaffleAddingParticipant(name: String)(hList: HList): HList = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleWithAddingParticipantFrom(name, hList)
    newRaffleHistory :: HNil
  }

  override protected def newStateForAddParticipant(name: String)(hList: HList): HList = {
    val newRaffleHistory = newRaffleHistoryForAddParticipantFrom(name, hList)
    newRaffleHistory :: HNil
  }

  override protected def newStateForRemoveParticipant(name: String)(hList: HList): HList = {
    val newRaffleHistory = newRaffleHistoryForRemoveParticipantFrom(name, hList)
    newRaffleHistory :: HNil
  }

  override protected def newStateForSelectWinner(hList: HList): HList = {
    val (winner, newRaffleHistory) = newRaffleHistoryForSelectWinnerFrom(hList)
    newRaffleHistory :: HNil
  }

  override protected def setState(hList: HList): M[Unit] = {
    val newRaffleHistory: RaffleHistory = hList.asInstanceOf[shapeless.::[RaffleHistory, HNil]].head
    setState1 {
      newRaffleHistory
    }
  }

  override protected def raffleCommandHandlerTemplate(command: RaffleCommand, commandHandlerBody: (RaffleCommand, HList) => M[Unit]): M[Unit] = {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      commandHandlerBody(command, currentRaffleHistory :: HNil)
    }
  }

  override protected def raffleCommandWithNameHandlerTemplate(command: RaffleCommandWithName, commandWithNameHandlerBody: (RaffleCommandWithName, HList) => M[Unit]): M[Unit] = {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      commandWithNameHandlerBody(command, currentRaffleHistory :: HNil)
    }
  }

}
