package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features.{FailureF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.id.RaffleId
import shapeless.{HList, HNil}

import scala.language.higherKinds

abstract class AbstractOptimizedRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]]]
  extends AbstractRaffleBehavior[M] {

  protected val implicitRaffleHistoryState1F = implicitly[State1F[RaffleHistory, M]]


  override protected def isRaffleCreated(hList: HList): Boolean = {
    val currentOptionalRaffleState: Option[RaffleState] = hList.getOptionalRaffleState
    currentOptionalRaffleState.isDefined
  }

  override protected def getRaffleId(hList: HList): RaffleId = {
    val currentOptionalRaffleState: Option[RaffleState] = hList.getOptionalRaffleState
    currentOptionalRaffleState.get.raffleId
  }

  override protected def participants(hList: HList): Seq[String] = {
    val currentOptionalRaffleState: Option[RaffleState] = hList.getOptionalRaffleState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants
  }

  override protected def hasParticipantBeenAdded(name: String, hList: HList): Boolean = {
    val currentOptionalRaffleState: Option[RaffleState] = hList.getOptionalRaffleState
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)
  }


  override protected def newStateForCreateRaffle(hList: HList): HList = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleFrom(hList)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleFrom(raffleId)
    newRaffleHistory :: newOptionalRaffleState :: HNil
  }

  override protected def newStateForCreateRaffleAddingParticipant(name: String)(hList: HList): HList = {
    val (raffleId, newRaffleHistory) = newRaffleHistoryForCreateRaffleAddingParticipantFrom(name, hList)
    val newOptionalRaffleState = newOptionalRaffleStateForCreateRaffleAddingParticipantFrom(name, raffleId)
    newRaffleHistory :: newOptionalRaffleState :: HNil
  }

  override protected def newStateForAddParticipant(name: String)(hList: HList): HList = {
    val newRaffleHistory = newRaffleHistoryForAddParticipantFrom(name: String, hList: HList)
    val newOptionalRaffleState = newOptionalRaffleStateForAddParticipantFrom(name, hList)
    newRaffleHistory :: newOptionalRaffleState :: HNil
  }

  override protected def newStateForRemoveParticipant(name: String)(hList: HList): HList = {
    val newRaffleHistory = newRaffleHistoryForRemoveParticipantFrom(name, hList)
    val newOptionalRaffleState = newOptionalRaffleStateForRemoveParticipantFrom(name, hList)
    newRaffleHistory :: newOptionalRaffleState :: HNil
  }

  override protected def newStateForSelectWinner(hList: HList): HList = {
    val (winner, newRaffleHistory) = newRaffleHistoryForSelectWinnerFrom(hList)
    val newOptionalRaffleState = newOptionalRaffleStateForSelectWinnerFrom(winner, hList)
    newRaffleHistory :: newOptionalRaffleState :: HNil
  }

}
