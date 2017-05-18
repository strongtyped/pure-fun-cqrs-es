package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.behavior.Behavior.ArgBlock
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.util.Util._

import scala.language.higherKinds

object OptimizedRaffleBehavior {

  type RaffleHistoryAndOptionalRaffleAggregateArgsBlock[M[+ _]] = ArgBlock[RaffleHistoryAndOptionalRaffleAggregateArgs, M]

}

import OptimizedRaffleBehavior._

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
  override protected def createRaffleBlock: RaffleHistoryAndOptionalRaffleAggregateArgsBlock[M] =
  blockTemplate({ args =>
    val raffleId = RaffleId.generate()
    mkRaffleHistoryAndOptionalRaffleAggregateArgs(
      updatedHistory(args, RaffleCreatedEvent(raffleId)),
      Some(Open(raffleId, List())))
  })

  override protected def createRaffleAddingParticipantBlock(name: String): RaffleHistoryAndOptionalRaffleAggregateArgsBlock[M] =
    blockTemplate({ args =>
      val raffleId = RaffleId.generate()
      mkRaffleHistoryAndOptionalRaffleAggregateArgs(
        updatedHistory(args, RaffleCreatedEvent(raffleId), ParticipantAddedEvent(name, raffleId)),
        Some(Open(raffleId, List())) map { currentRaffleState =>
          currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
        })
    })

  override protected def addParticipantBlock(name: String): RaffleHistoryAndOptionalRaffleAggregateArgsBlock[M] =
    blockTemplate({ args =>
      mkRaffleHistoryAndOptionalRaffleAggregateArgs(
        updatedHistory(args, ParticipantAddedEvent(name, getRaffleId(args))),
        args.getOptionalAggregate map { currentRaffleState =>
          val openState = currentRaffleState.asInstanceOf[Open]
          openState.copy(participants = openState.participants.add(name))
        })
    })

  override protected def removeParticipantBlock(name: String): RaffleHistoryAndOptionalRaffleAggregateArgsBlock[M] =
    blockTemplate({ args =>
      mkRaffleHistoryAndOptionalRaffleAggregateArgs(
        updatedHistory(args, ParticipantRemovedEvent(name, getRaffleId(args))),
        args.getOptionalAggregate map { currentRaffleState =>
          val openState = currentRaffleState.asInstanceOf[Open]
          openState.copy(participants = openState.participants.remove(name))
        })
    })

  override protected def selectWinnerBlock: RaffleHistoryAndOptionalRaffleAggregateArgsBlock[M] =
    blockTemplate({ args =>
      val raffleWinner = winner(args)
      mkRaffleHistoryAndOptionalRaffleAggregateArgs(
        updatedHistory(args, WinnerSelectedEvent(winner(args), OffsetDateTime.now, getRaffleId(args))),
        args.getOptionalAggregate map { currentRaffleState =>
          Closed(currentRaffleState.raffleId, raffleWinner)
        })
    })

}
