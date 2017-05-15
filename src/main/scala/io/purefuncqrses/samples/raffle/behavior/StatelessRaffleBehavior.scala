package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}

import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}
import shapeless.{::, HList, HNil}

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


  override protected def setState(hList: HList): M[Unit] = {
    val newRaffleHistory: RaffleHistory = hList._1
    setState1 {
      newRaffleHistory
    }
  }


  override protected def newStateForCreateRaffle(hList: HList): HList = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(hList)
    newRaffleHistory :: HNil
  }

  override protected def newStateForCreateRaffleAddingParticipant(name: String)(hList: HList): HList = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleAddingParticipantFrom(name, hList)
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


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    getState1(()) flatMap { currentRaffleHistory =>
      handlerBody(command, currentRaffleHistory :: HNil)
    }
  }

}
