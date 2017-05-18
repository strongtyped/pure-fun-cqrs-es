package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId

import io.purefuncqrses.util.Util._

import scala.language.higherKinds

abstract class OptimizedRaffleBehavior[S <: RaffleState, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]]
  extends RaffleBehavior[RaffleHistoryAndOptionalRaffleStateArgs, S, M] {

  //
  // basic functions
  //
  override protected def getRaffleId(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleId = {
    val currentOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleState.get.raffleId
  }

  override protected def participants(args: RaffleHistoryAndOptionalRaffleStateArgs): Seq[String] = {
    val currentOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants
  }


  //
  // basic predicates
  //
  override protected def isRaffleCreated(args: RaffleHistoryAndOptionalRaffleStateArgs): Boolean = {
    val currentOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleState.isDefined
  }

  override protected def hasParticipantBeenAdded(name: String, args: RaffleHistoryAndOptionalRaffleStateArgs): Boolean = {
    val currentOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)
  }

  //
  // more complex functions
  //
  override protected def newArgsForCreateRaffle(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = newHistoryFor(raffleId, args, RaffleCreatedEvent(raffleId))
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForCreateRaffleAddingParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = newHistoryFor(raffleId, args, RaffleCreatedEvent(raffleId), ParticipantAddedEvent(name, raffleId))
    val tmpOptionalRaffleState = Some(OpenState(raffleId, List()))
    val newOptionalRaffleState = tmpOptionalRaffleState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForAddParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val newRaffleHistory = newHistoryFor(args, ParticipantAddedEvent(name, getRaffleId(args)))
    val currentOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForRemoveParticipant(name: String)(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val newRaffleHistory = newHistoryFor(args, ParticipantRemovedEvent(name, getRaffleId(args)))
    val currentOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

  override protected def newArgsForSelectWinner(args: RaffleHistoryAndOptionalRaffleStateArgs): RaffleHistoryAndOptionalRaffleStateArgs = {
    val raffleWinner = winner(args)
    val newRaffleHistory = newHistoryFor(args, WinnerSelectedEvent(winner(args), OffsetDateTime.now, getRaffleId(args)))
    val currentOptionalRaffleState: Option[RaffleAggregateState] = args.getOptionalAggregateState
    val newOptionalRaffleState = currentOptionalRaffleState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, raffleWinner)
    }
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    raffleHistoryAndOptionalRaffleStateArgs(newRaffleHistory, newOptionalRaffleState)
  }

}
