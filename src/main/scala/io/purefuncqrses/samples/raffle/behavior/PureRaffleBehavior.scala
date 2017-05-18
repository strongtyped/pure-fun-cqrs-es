package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.behavior.HistoryArg
import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.RaffleHistory

import scala.language.higherKinds

class PureRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[HistoryState, ?[_]]]
  extends RaffleBehavior[RaffleHistoryArg, HistoryState, M] {

  //
  // basic functions
  //
  override protected def getRaffleId(args: RaffleHistoryArg): RaffleId = {
    val currentRaffleHistory: RaffleHistory = args.getHistory
    currentRaffleHistory.head.asInstanceOf[RaffleCreatedEvent].raffleId
  }

  override protected def participants(args: RaffleHistoryArg): Seq[String] = {
    val currentRaffleHistory: RaffleHistory = args.getHistory
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


  //
  // basic predicates
  //
  override protected def isRaffleCreated(args: RaffleHistoryArg): Boolean = {
    val currentRaffleHistory: RaffleHistory = args.getHistory
    currentRaffleHistory.nonEmpty
  }

  override protected def hasParticipantBeenAdded(name: String, args: RaffleHistoryArg): Boolean = {
    val currentRaffleHistory: RaffleHistory = args.getHistory
    val numberOfTimesAdded =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantAddedEvent] && raffleEvent.asInstanceOf[ParticipantAddedEvent].name == name)
    val numberOfTimesRemoved =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantRemovedEvent] && raffleEvent.asInstanceOf[ParticipantRemovedEvent].name == name)
    numberOfTimesAdded > numberOfTimesRemoved
  }


  //
  // more complex functions
  //
  override protected def newArgsForCreateRaffle(args: RaffleHistoryArg): RaffleHistoryArg = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = newHistoryFor(raffleId, args, RaffleCreatedEvent(raffleId))
    HistoryArg[RaffleEvent, RaffleState](newRaffleHistory)
  }

  override protected def newArgsForCreateRaffleAddingParticipant(name: String)(args: RaffleHistoryArg): RaffleHistoryArg = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = newHistoryFor(raffleId, args, RaffleCreatedEvent(raffleId), ParticipantAddedEvent(name, raffleId))
    HistoryArg[RaffleEvent, RaffleState](newRaffleHistory)
  }

  override protected def newArgsForAddParticipant(name: String)(args: RaffleHistoryArg): RaffleHistoryArg = {
    val newRaffleHistory = newHistoryFor(args, ParticipantAddedEvent(name, getRaffleId(args)))
    HistoryArg[RaffleEvent, RaffleState](newRaffleHistory)
  }

  override protected def newArgsForRemoveParticipant(name: String)(args: RaffleHistoryArg): RaffleHistoryArg = {
    val newRaffleHistory = newHistoryFor(args, ParticipantRemovedEvent(name, getRaffleId(args)))
    HistoryArg[RaffleEvent, RaffleState](newRaffleHistory)
  }

  override protected def newArgsForSelectWinner(args: RaffleHistoryArg): RaffleHistoryArg = {
    val raffleWinner = winner(args)
    val newRaffleHistory = newHistoryFor(args, WinnerSelectedEvent(raffleWinner, OffsetDateTime.now, getRaffleId(args)))
    HistoryArg[RaffleEvent, RaffleState](newRaffleHistory)
  }

}
