package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.{Handler, State}
import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{HandlerBody, RaffleHistory}

import scala.language.higherKinds

class StatelessRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[State, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  private val implicitRaffleHistoryState1F = implicitly[State1F[State, M]]

  import implicitRaffleHistoryState1F._


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
    case History_Arg(_) =>
      val newRaffleHistory: RaffleHistory = args.getRaffleHistory
      setState {
        History_Arg(newRaffleHistory)
      }
    case _ =>
      failure(new IllegalStateException(s"$args is not a history argument"))
  }


  override protected def newStateForCreateRaffle(args: Args): Args = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(args)
    History_Arg(newRaffleHistory)
  }

  override protected def newStateForCreateRaffleAddingParticipant(name: String)(args: Args): Args = {
    val (_, newRaffleHistory) = newRaffleHistoryForCreateRaffleAddingParticipantFrom(name, args)
    History_Arg(newRaffleHistory)
  }

  override protected def newStateForAddParticipant(name: String)(args: Args): Args = {
    val newRaffleHistory = newRaffleHistoryForAddParticipantFrom(name, args)
    History_Arg(newRaffleHistory)
  }

  override protected def newStateForRemoveParticipant(name: String)(args: Args): Args = {
    val newRaffleHistory = newRaffleHistoryForRemoveParticipantFrom(name, args)
    History_Arg(newRaffleHistory)
  }

  override protected def newStateForSelectWinner(args: Args): Args = {
    val (winner, newRaffleHistory) = newRaffleHistoryForSelectWinnerFrom(args)
    History_Arg(newRaffleHistory)
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    getState(()) flatMap {
      case History_Arg(currentRaffleHistory) =>
      handlerBody(command, History_Arg(currentRaffleHistory))
      case args =>
        failure(new IllegalStateException(s"$args is not a history argument"))
    }
  }

}
