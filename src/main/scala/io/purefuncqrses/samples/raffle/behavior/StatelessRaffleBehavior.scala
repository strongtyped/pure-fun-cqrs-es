package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}

import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class StatelessRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  private val implicitStateF = implicitly[StateF[State, M]]

  import implicitStateF._


  override protected def isRaffleCreated(args: Args): Boolean = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    currentRaffleHistory.nonEmpty
  }

  override protected def getRaffleId(args: Args): RaffleId = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    currentRaffleHistory.head.asInstanceOf[RaffleCreatedEvent].raffleId
  }

  override protected def participants(args: Args): Seq[String] = {
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

  override protected def hasParticipantBeenAdded(name: String, args: Args): Boolean = {
    val currentRaffleHistory: RaffleHistory = args.getRaffleHistory
    val numberOfTimesAdded =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantAddedEvent] && raffleEvent.asInstanceOf[ParticipantAddedEvent].name == name)
    val numberOfTimesRemoved =
      currentRaffleHistory.count(raffleEvent => raffleEvent.isInstanceOf[ParticipantRemovedEvent] && raffleEvent.asInstanceOf[ParticipantRemovedEvent].name == name)
    numberOfTimesAdded > numberOfTimesRemoved
  }


  override protected def setState(args: Args): M[Unit] = args match {
    case state : HistoryArg =>
      write {
        state
      }
    case state =>
      failure(new IllegalStateException(s"$args is not a 'history' argument"))
  }


  override protected def newStateForCreateRaffle(args: Args): Args = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForCreateRaffleAddingParticipant(name: String)(args: Args): Args = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleAddingParticipantFrom(name, args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForAddParticipant(name: String)(args: Args): Args = {
    val newRaffleHistory = newRaffleHistoryForAddParticipantFrom(name, args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForRemoveParticipant(name: String)(args: Args): Args = {
    val newRaffleHistory = newRaffleHistoryForRemoveParticipantFrom(name, args)
    HistoryArg(newRaffleHistory)
  }

  override protected def newStateForSelectWinner(args: Args): Args = {
    val (winner, newRaffleHistory) = newRaffleHistoryForSelectWinnerFrom(args)
    HistoryArg(newRaffleHistory)
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      case state : HistoryArg =>
      handlerBody(command, state)
      case state =>
        failure(new IllegalStateException(s"$state is not a 'history' state"))
    }
  }

}
