package io.purefuncqrses.samples.raffle.behavior

import java.time.OffsetDateTime

import io.purefuncqrses.util.Util._
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.commands._
import io.purefuncqrses.samples.raffle.events._
import io.purefuncqrses.samples.raffle.id.RaffleId
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{PartialRaffleCommandHandler, RaffleHistory}

import scala.language.higherKinds
import scala.util.Random

class PureStatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]] : State2F[Option[RaffleState], ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  private val implicitRaffleHistoryStateF = implicitly[State1F[RaffleHistory, M]]

  import implicitRaffleHistoryStateF._

  private val implicitNestedOptionalRaffleStateStateF = implicitly[State2F[Option[RaffleState], M]]

  import implicitNestedOptionalRaffleStateStateF._

  private def isRaffleCreated(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    currentOptionalRaffleState.isDefined

  private def getRaffleId(currentOptionalRaffleState: Option[RaffleState]): RaffleId =
    currentOptionalRaffleState.get.raffleId

  private def hasParticipantBeenAdded(name: String, currentOptionalRaffleState: Option[RaffleState]): Boolean =
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)

  private def participants(currentOptionalRaffleState: Option[RaffleState]): Seq[String] =
    currentOptionalRaffleState.get.asInstanceOf[OpenState].participants

  private def createRaffleCondition(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    !isRaffleCreated(currentOptionalRaffleState)

  private def createRaffleAddingParticipantCondition(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    !isRaffleCreated(currentOptionalRaffleState)

  private def addParticipantCondition(name: String, currentOptionalRaffleState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalRaffleState) && !hasParticipantBeenAdded(name, currentOptionalRaffleState)

  private def removeParticipantCondition(name: String, currentOptionalRaffleState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalRaffleState) && hasParticipantBeenAdded(name, currentOptionalRaffleState)

  private def selectWinnerCondition(currentOptionalRaffleState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalRaffleState) && participants(currentOptionalRaffleState).nonEmpty

  private def createRaffleBlock(currentRaffleHistory: RaffleHistory): M[Unit] = {
    val raffleId = RaffleId.generate()
    val newRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    println(s"\nnew raffle history = $newRaffleHistory")
    val newOptionalRaffleState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional raffle state = $newOptionalRaffleState")
    setState2 {
      newOptionalRaffleState
    } flatSeq {
      setState1 {
        newRaffleHistory
      }
    }
  }

  private def createRaffleAddingParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val raffleId = RaffleId.generate()
    val tmpRaffleHistory = currentRaffleHistory :+ RaffleCreatedEvent(raffleId)
    val newRaffleHistory = tmpRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    val tmpOptionalState = Some(OpenState(raffleId, List()))
    val newOptionalState = tmpOptionalState map { currentRaffleState =>
      currentRaffleState.copy(participants = currentRaffleState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalState")
    setState2 {
      newOptionalState
    } flatSeq {
      setState1 {
        newRaffleHistory
      }
    }
  }

  private def addParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val newOptionalState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional raffle state = $newOptionalState")
    val newRaffleHistory = currentRaffleHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    setState2 {
      newOptionalState
    } flatSeq {
      setState1 {
        newRaffleHistory
      }
    }
  }

  private def removeParticipantBlock(name: String, currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]) = {
    val newOptionalState = currentOptionalRaffleState map { currentRaffleState =>
      val openState = currentRaffleState.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional raffle state = $newOptionalState")
    val newRaffleHistory = currentRaffleHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    setState2 {
      newOptionalState
    } flatSeq {
      setState1 {
        newRaffleHistory
      }
    }
  }

  private def selectWinnerBlock(currentRaffleHistory: RaffleHistory, currentOptionalRaffleState: Option[RaffleState]): M[Unit] = {
    val currentParticipants = participants(currentOptionalRaffleState)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newRaffleHistory = currentRaffleHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(currentOptionalRaffleState))
    println(s"\nnew raffle history = $newRaffleHistory")
    val newOptionalState = currentOptionalRaffleState map { currentRaffleState =>
      ClosedState(currentRaffleState.raffleId, winner)
    }
    println(s"\nnew optional raffle state = $newOptionalState")
    setState2 {
      newOptionalState
    } flatSeq {
      setState1 {
        newRaffleHistory
      }
    }
  }

  override protected lazy val createRaffleCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        getState2(()) flatMap { currentOptionalRaffleState =>
          println(s"\ncurrent raffle history = $currentRaffleHistory")
          println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
          if (createRaffleCondition(currentOptionalRaffleState)) {
            createRaffleBlock(currentRaffleHistory)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
          }
        }
      }
  }

  override protected lazy val createRaffleAddingParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        getState2(()) flatMap { currentOptionalRaffleState =>
          println(s"\ncurrent raffle history = $currentRaffleHistory")
          println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
          if (createRaffleAddingParticipantCondition(currentOptionalRaffleState)) {
            createRaffleAddingParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
          }
        }
      }
  }

  override protected lazy val addParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        getState2(()) flatMap { currentOptionalRaffleState =>
          println(s"\ncurrent raffle history = $currentRaffleHistory")
          println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
          if (addParticipantCondition(command.name, currentOptionalRaffleState)) {
            addParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
          }
        }
      }
  }

  override protected lazy val removeParticipantCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        getState2(()) flatMap { currentOptionalRaffleState =>
          println(s"\ncurrent raffle history = $currentRaffleHistory")
          println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
          if (removeParticipantCondition(command.name, currentOptionalRaffleState)) {
            removeParticipantBlock(command.name, currentRaffleHistory, currentOptionalRaffleState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
          }
        }
      }
  }

  override protected lazy val selectWinnerCommandHandler: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      println(s"\ncase $command =>")
      getState1(()) flatMap { currentRaffleHistory =>
        getState2(()) flatMap { currentOptionalRaffleState =>
          println(s"\ncurrent raffle history = $currentRaffleHistory")
          println(s"\ncurrent optional raffle state = $currentOptionalRaffleState")
          if (selectWinnerCondition(currentOptionalRaffleState)) {
            selectWinnerBlock(currentRaffleHistory, currentOptionalRaffleState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentRaffleHistory"))
          }
        }
      }
  }

}
