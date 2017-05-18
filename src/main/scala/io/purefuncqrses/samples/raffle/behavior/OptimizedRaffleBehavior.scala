package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId

import io.purefuncqrses.util.Util._

import scala.language.higherKinds

abstract class OptimizedRaffleBehavior[S <: RaffleState, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]]
  extends RaffleBehavior[RaffleHistoryAndOptionalRaffleAggregateArgs, S, M] {

  //
  // basic functions
  //

  override protected def getRaffleId(args: RaffleHistoryAndOptionalRaffleAggregateArgs): RaffleId = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregate] = args.getOptionalAggregate
    currentOptionalRaffleAggregateState.get.raffleId
  }

  override protected def participants(args: RaffleHistoryAndOptionalRaffleAggregateArgs): Seq[String] = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregate] = args.getOptionalAggregate
    currentOptionalRaffleAggregateState.get.asInstanceOf[Open].participants
  }


  //
  // basic predicates
  //

  override protected def isRaffleCreated(args: RaffleHistoryAndOptionalRaffleAggregateArgs): Boolean = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregate] = args.getOptionalAggregate
    currentOptionalRaffleAggregateState.isDefined
  }

  override protected def hasParticipantBeenAdded(name: String, args: RaffleHistoryAndOptionalRaffleAggregateArgs): Boolean = {
    val currentOptionalRaffleAggregateState: Option[RaffleAggregate] = args.getOptionalAggregate
    currentOptionalRaffleAggregateState.get.asInstanceOf[Open].participants.contains(name)
  }

  //
  // blocks
  //
  override protected def createRaffleBlock(args: RaffleHistoryAndOptionalRaffleAggregateArgs): M[Unit] = {
    val raffleId = RaffleId.generate()
    val raffleHistoryAndOptionalRaffleAggregateArgs = mkRaffleHistoryAndOptionalRaffleAggregateArgs(
      updateHistory(args, RaffleCreatedEvent(raffleId)),
      Some(Open(raffleId, List())))
    println(s"\n$raffleHistoryAndOptionalRaffleAggregateArgs")
    setStateFromArgs(raffleHistoryAndOptionalRaffleAggregateArgs)
  }

  override protected def createRaffleAddingParticipantBlock(name: String)(args: RaffleHistoryAndOptionalRaffleAggregateArgs): M[Unit] = {
    val raffleId = RaffleId.generate()
    val raffleHistoryAndOptionalRaffleAggregateArgs = mkRaffleHistoryAndOptionalRaffleAggregateArgs(
      updateHistory(args, RaffleCreatedEvent(raffleId), ParticipantAddedEvent(name, raffleId)),
      Some(Open(raffleId, List())) map { currentRaffleState =>
        currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
      })
    println(s"\n$raffleHistoryAndOptionalRaffleAggregateArgs")
    setStateFromArgs(raffleHistoryAndOptionalRaffleAggregateArgs)
  }

  override protected def addParticipantBlock(name: String)(args: RaffleHistoryAndOptionalRaffleAggregateArgs): M[Unit] = {
    val raffleHistoryAndOptionalRaffleAggregateArgs = mkRaffleHistoryAndOptionalRaffleAggregateArgs(
      updateHistory(args, ParticipantAddedEvent(name, getRaffleId(args))),
      args.getOptionalAggregate map { currentRaffleState =>
        val openState = currentRaffleState.asInstanceOf[Open]
        openState.copy(participants = openState.participants.add(name))
      })
    println(s"\n$raffleHistoryAndOptionalRaffleAggregateArgs")
    setStateFromArgs(raffleHistoryAndOptionalRaffleAggregateArgs)
  }

  override protected def removeParticipantBlock(name: String)(args: RaffleHistoryAndOptionalRaffleAggregateArgs): M[Unit] = {
    val raffleHistoryAndOptionalRaffleAggregateArgs = mkRaffleHistoryAndOptionalRaffleAggregateArgs(
      updateHistory(args, ParticipantRemovedEvent(name, getRaffleId(args))),
      args.getOptionalAggregate map { currentRaffleState =>
        val openState = currentRaffleState.asInstanceOf[Open]
        openState.copy(participants = openState.participants.remove(name))
      })
    println(s"\n$raffleHistoryAndOptionalRaffleAggregateArgs")
    setStateFromArgs(raffleHistoryAndOptionalRaffleAggregateArgs)
  }


  override protected def selectWinnerBlock(args: RaffleHistoryAndOptionalRaffleAggregateArgs): M[Unit] = {
    val raffleWinner = winner(args)
    val raffleHistoryAndOptionalRaffleAggregateArgs = mkRaffleHistoryAndOptionalRaffleAggregateArgs(
      updateHistory(args, WinnerSelectedEvent(winner(args), OffsetDateTime.now, getRaffleId(args))),
      args.getOptionalAggregate map { currentRaffleState =>
        Closed(currentRaffleState.raffleId, raffleWinner)
      })
    println(s"\n$raffleHistoryAndOptionalRaffleAggregateArgs")
    setStateFromArgs(raffleHistoryAndOptionalRaffleAggregateArgs)
  }

}
