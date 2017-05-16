package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

abstract class AbstractOptimizedRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  protected val implicitStateF = implicitly[StateF[State, M]]


  override protected def isRaffleCreated(args: Args): Boolean = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    currentOptionalRaffleState.isDefined
  }

  override protected def getRaffleId(args: Args): RaffleId = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    currentOptionalRaffleState.get.raffleId
  }

  override protected def participants(args: Args): Seq[String] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants
  }

  override protected def hasParticipantBeenAdded(name: String, args: Args): Boolean = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalRaffleState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)
  }


  override protected def newStateForCreateRaffle(args: Args): Args = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(args)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleFrom(raffleId)
    HistoryAndOptionalStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newStateForCreateRaffleAddingParticipant(name: String)(args: Args): Args = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleAddingParticipantFrom(name, args)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleAddingParticipantFrom(name, raffleId)
    HistoryAndOptionalStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newStateForAddParticipant(name: String)(args: Args): Args = {
    val newRaffleHistory = newRaffleHistoryForAddParticipantFrom(name, args)
    val newOptionalRaffleState = newOptionalRaffleStateForAddParticipantFrom(name, args)
    HistoryAndOptionalStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newStateForRemoveParticipant(name: String)(args: Args): Args = {
    val newRaffleHistory = newRaffleHistoryForRemoveParticipantFrom(name, args)
    val newOptionalRaffleState = newOptionalRaffleStateForRemoveParticipantFrom(name, args)
    HistoryAndOptionalStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newStateForSelectWinner(args: Args): Args = {
    val (winner, newRaffleHistory) = newRaffleHistoryForSelectWinnerFrom(args)
    val newOptionalRaffleState = newOptionalRaffleStateForSelectWinnerFrom(winner, args)
    HistoryAndOptionalStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

}
