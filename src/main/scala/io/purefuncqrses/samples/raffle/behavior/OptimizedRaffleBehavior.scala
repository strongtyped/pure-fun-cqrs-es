package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId

import io.purefuncqrses.util.Util._

import scala.language.higherKinds

abstract class OptimizedRaffleBehavior[S <: RaffleState, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]]
  extends RaffleBehavior[RaffleHistoryAndOptionalRaffleAggregateStateArgs, S, M] {

  //
  // basic functions
  //
  override protected def getRaffleId(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): RaffleId = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleAggregateState.get.raffleId
  }

  override protected def participants(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): Seq[String] = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleAggregateState.get.asInstanceOf[OpenState].participants
  }


  //
  // basic predicates
  //
  override protected def isRaffleCreated(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): Boolean = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleAggregateState.isDefined
  }

  override protected def hasParticipantBeenAdded(name: String, args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): Boolean = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleAggregateState.get.asInstanceOf[OpenState].participants.contains(name)
  }

  //
  // more complex functions
  //
  override protected def newArgsForCreateRaffle(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): RaffleHistoryAndOptionalRaffleAggregateStateArgs = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = newHistoryFor(raffleId, args, RaffleCreatedEvent(raffleId))
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleAggregateStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForCreateRaffleAddingParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): RaffleHistoryAndOptionalRaffleAggregateStateArgs = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = newHistoryFor(raffleId, args, RaffleCreatedEvent(raffleId), ParticipantAddedEvent(name, raffleId))
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleAggregateStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForAddParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): RaffleHistoryAndOptionalRaffleAggregateStateArgs = {
    val newRaffleHistory = newHistoryFor(args, ParticipantAddedEvent(name, getRaffleId(args)))
    val currentOptionalRaffleAggregateState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    val newOptionalRaffleState = currentOptionalRaffleAggregateState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleAggregateStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForRemoveParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): RaffleHistoryAndOptionalRaffleAggregateStateArgs = {
    val newRaffleHistory = newHistoryFor(args, ParticipantRemovedEvent(name, getRaffleId(args)))
    val currentOptionalRaffleAggregateState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    val newOptionalRaffleState = currentOptionalRaffleAggregateState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleAggregateStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForSelectWinner(args: RaffleHistoryAndOptionalRaffleAggregateStateArgs): RaffleHistoryAndOptionalRaffleAggregateStateArgs = {
    val raffleWinner = winner(args)
    val newRaffleHistory = newHistoryFor(args, WinnerSelectedEvent(winner(args), OffsetDateTime.now, getRaffleId(args)))
    val currentOptionalRaffleAggregateState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    val newOptionalRaffleState = currentOptionalRaffleAggregateState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, raffleWinner)
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleAggregateStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

}
