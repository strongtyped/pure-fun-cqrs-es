package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}

import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class PureRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[HistoryState, ?[_]]]
  extends RaffleBehavior[HistoryArg, HistoryState, M] {

  private val implicitStateF = implicitly[StateF[HistoryState, M]]

  import implicitStateF._


  override protected def isRaffleCreated(args: HistoryArg): Boolean = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    currentRaffleHistory.nonEmpty
  }

  override protected def getRaffleId(args: HistoryArg): RaffleId = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    currentRaffleHistory.head.asInstanceOf[RaffleCreatedEvent].raffleId
  }

  override protected def participants(args: HistoryArg): Seq[String] = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
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

  override protected def hasParticipantBeenAdded(name: String, args: HistoryArg): Boolean = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val numberOfTimesAdded =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantAddedEvent] && raffleEvent.asInstanceOf[ParticipantAddedEvent].name == name)
    val numberOfTimesRemoved =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantRemovedEvent] && raffleEvent.asInstanceOf[ParticipantRemovedEvent].name == name)
    numberOfTimesAdded > numberOfTimesRemoved
  }


  override protected def setState(args: HistoryArg): M[Unit] = {
    val newState = args
    write {
      newState
    }
  }


  override protected def newStateForCreateRaffle(args: HistoryArg): HistoryArg = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForCreateRaffleAddingParticipant(name: String)(args: HistoryArg): HistoryArg = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleAddingParticipantFrom(name, args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForAddParticipant(name: String)(args: HistoryArg): HistoryArg = {
    val newRaffleHistory = newRaffleHistoryForAddParticipantFrom(name, args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForRemoveParticipant(name: String)(args: HistoryArg): HistoryArg = {
    val newRaffleHistory = newRaffleHistoryForRemoveParticipantFrom(name, args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForSelectWinner(args: HistoryArg): HistoryArg = {
    val (_, newRaffleHistory) = newRaffleHistoryForSelectWinnerFrom(args)
    HistoryArg(newRaffleHistory)
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[HistoryArg, Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      state =>
        handlerBody(command, state)
    }
  }

}
