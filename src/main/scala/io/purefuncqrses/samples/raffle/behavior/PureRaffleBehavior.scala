package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.{RaffleHistory, RaffleHistoryCommandHandlerBlock}

import scala.language.higherKinds

class PureRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistoryState, ?[_]]]
  extends RaffleBehavior[RaffleHistoryArg, RaffleHistoryState, M] {

  //
  // basic functions
  //

  override protected def getRaffleId(args: RaffleHistoryArg): RaffleId = {
    val currentRaffleHistory: RaffleHistory = args.getHistory
    currentRaffleHistory.head match {
      case event: RaffleCreatedEvent =>
        event.raffleId
      case event =>
        sys.error(s"$event is not a raffle created event (should never happen)")
    }
  }

  override protected def participants(args: RaffleHistoryArg): Seq[String] = {
    val currentRaffleHistory: RaffleHistory = args.getHistory
    currentRaffleHistory.tail.foldLeft(List[String]()) { (participants, raffleEvent) =>
      raffleEvent match {
        case ParticipantAddedEvent(name, _) =>
          participants.add(name)
        case ParticipantRemovedEvent(name, _) =>
          participants.remove(name)
        case event =>
          sys.error(s"$event is not applicable for non-existing raffle (should never happen)")
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
  // blocks
  //

  override protected val createRaffleBlock: RaffleHistoryCommandHandlerBlock[M] =
    blockTemplate({ args =>
      val raffleId = RaffleId.generate()
      mkRaffleHistoryArg(updatedHistory(args, RaffleCreatedEvent(raffleId)))
    })

  override protected def createRaffleAddingParticipantBlock(name: String): RaffleHistoryCommandHandlerBlock[M] =
    blockTemplate({ args =>
      val raffleId = RaffleId.generate()
      mkRaffleHistoryArg(updatedHistory(args, RaffleCreatedEvent(raffleId), ParticipantAddedEvent(name, raffleId)))
    })

  override protected def addParticipantBlock(name: String): RaffleHistoryCommandHandlerBlock[M] =
    blockTemplate({ args =>
      mkRaffleHistoryArg(updatedHistory(args, ParticipantAddedEvent(name, getRaffleId(args))))
    })

  override protected def removeParticipantBlock(name: String): RaffleHistoryCommandHandlerBlock[M] =
    blockTemplate({ args =>
      mkRaffleHistoryArg(updatedHistory(args, ParticipantRemovedEvent(name, getRaffleId(args))))
    })

  override protected val selectWinnerBlock: RaffleHistoryCommandHandlerBlock[M] =
    blockTemplate({ args =>
      mkRaffleHistoryArg(updatedHistory(args, WinnerSelectedEvent(winner(args), OffsetDateTime.now, getRaffleId(args))))
    })

}
