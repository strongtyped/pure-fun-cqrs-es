package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.HistoryAndOptionalAggregateStateArgs
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

abstract class OptimizedRaffleBehavior[S <: State, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]]
  extends RaffleBehavior[RaffleHistoryAndOptionalRaffleStateArgs, S, M] {

  //
  // basic functions
  //
  override protected def getRaffleId(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleId = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalAggregateState
    currentOptionalRaffleState.get.raffleId
  }

  override protected def participants(args: RaffleHistoryAndOptionalRaffleStateArgs): Seq[String] = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalAggregateState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants
  }


  //
  // basic predicates
  //
  override protected def isRaffleCreated(args: RaffleHistoryAndOptionalRaffleStateArgs): Boolean = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalAggregateState
    currentOptionalRaffleState.isDefined
  }

  override protected def hasParticipantBeenAdded(name: String, args: RaffleHistoryAndOptionalRaffleStateArgs): Boolean = {
    val currentOptionalRaffleState: Option[RaffleState] = args.getOptionalAggregateState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)
  }


  //
  // more complex functions
  //
  override protected def newArgsForCreateRaffle(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(args)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleFrom(raffleId)
    HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleState](newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForCreateRaffleAddingParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleAddingParticipantFrom(name, args)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleAddingParticipantFrom(name, raffleId)
    HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleState](newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForAddParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val newRaffleHistory = newRaffleHistoryForAddParticipantFrom(name, args)
    val newOptionalRaffleState = newOptionalRaffleStateForAddParticipantFrom(name, args)
    HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleState](newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForRemoveParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val newRaffleHistory = newRaffleHistoryForRemoveParticipantFrom(name, args)
    val newOptionalRaffleState = newOptionalRaffleStateForRemoveParticipantFrom(name, args)
    HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleState](newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForSelectWinner(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val (winner, newRaffleHistory) = newRaffleHistoryForSelectWinnerFrom(args)
    val newOptionalRaffleState = newOptionalRaffleStateForSelectWinnerFrom(winner, args)
    HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleState](newRaffleHistory, newOptionalRaffleState)
  }

}
