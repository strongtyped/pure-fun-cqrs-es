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

class PureStatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]] : NestedStateF[Option[RaffleState], ?[_]]]
  extends AbstractRaffleBehavior[M] {

  import implicitFailureF._

  private val implicitRaffleHistoryStateF = implicitly[StateF[RaffleHistory, M]]

  import implicitRaffleHistoryStateF._

  private val implicitNestedOptionalRaffleStateStateF = implicitly[NestedStateF[Option[RaffleState], M]]

  import implicitNestedOptionalRaffleStateStateF._

  private def isRaffleCreated(optionalRaffleState: Option[RaffleState]): Boolean =
    optionalRaffleState.isDefined

  private def getRaffleId(optionalRaffleState: Option[RaffleState]): RaffleId =
    optionalRaffleState.get.raffleId

  private def hasParticipantBeenAdded(name: String, optionalRaffleState: Option[RaffleState]): Boolean =
    optionalRaffleState.get.asInstanceOf[OpenState].participants.contains(name)

  private def participants(optionalRaffleState: Option[RaffleState]): Seq[String] =
    optionalRaffleState.get.asInstanceOf[OpenState].participants

  private def createRaffleCondition(currentOptionalState: Option[RaffleState]): Boolean =
    !isRaffleCreated(currentOptionalState)

  private def createRaffleAddingParticipantCondition(currentOptionalState: Option[RaffleState]): Boolean =
    !isRaffleCreated(currentOptionalState)

  private def addParticipantCondition(name: String, currentOptionalState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalState) && !hasParticipantBeenAdded(name, currentOptionalState)

  private def removeParticipantCondition(name: String, currentOptionalState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalState) && hasParticipantBeenAdded(name, currentOptionalState)

  private def selectWinnerCondition(currentOptionalState: Option[RaffleState]): Boolean =
    isRaffleCreated(currentOptionalState) && participants(currentOptionalState).nonEmpty

  private def createRaffleBlock(currentHistory: RaffleHistory): M[Unit] = {
    val raffleId = RaffleId.generate()
    val newOptionalState = Some(OpenState(raffleId, List()))
    println(s"\nnew optional state = $newOptionalState")
    setNestedState(newOptionalState) flatMap { _ =>
      setState {
        val newHistory = currentHistory :+ RaffleCreatedEvent(raffleId)
        println(s"\nnew history = $newHistory")
        newHistory
      }
    }
  }

  private def createRaffleAddingParticipantBlock(name: String, currentHistory: RaffleHistory, currentOptionalState: Option[RaffleState]) = {
    val raffleId = RaffleId.generate()
    val tmpOptionalState = Some(OpenState(raffleId, List()))
    val newOptionalState = tmpOptionalState map { state =>
      state.copy(participants = state.participants.add(name))
    }
    println(s"\nnew optional state = $newOptionalState")
    setNestedState(newOptionalState) flatMap { _ =>
      setState {
        val tmpHistory = currentHistory :+ RaffleCreatedEvent(raffleId)
        val newHistory = tmpHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalState))
        println(s"\nnew history = $newHistory")
        newHistory
      }
    }
  }

  private def addParticipantBlock(name: String, currentHistory: RaffleHistory, currentOptionalState: Option[RaffleState]) = {
    val newOptionalState = currentOptionalState map { state =>
      val openState = state.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.add(name))
    }
    println(s"\nnew optional state = $newOptionalState")
    setNestedState(newOptionalState) flatMap { _ =>
      setState {
        val newHistory = currentHistory :+ ParticipantAddedEvent(name, getRaffleId(currentOptionalState))
        println(s"\nnew history = $newHistory")
        newHistory
      }
    }
  }

  private def removeParticipantBlock(name: String, currentHistory: RaffleHistory, currentOptionalState: Option[RaffleState]) = {
    val newOptionalState = currentOptionalState map { state =>
      val openState = state.asInstanceOf[OpenState]
      openState.copy(participants = openState.participants.remove(name))
    }
    println(s"\nnew optional state = $newOptionalState")
    setNestedState(newOptionalState) flatMap { _ =>
      setState {
        val newHistory = currentHistory :+ ParticipantRemovedEvent(name, getRaffleId(currentOptionalState))
        println(s"\nnew history = $newHistory")
        newHistory
      }
    }
  }

  private def selectWinnerBlock(currentHistory: RaffleHistory, currentOptionalState: Option[RaffleState]): M[Unit] = {
    val currentParticipants = participants(currentOptionalState)
    val winner = currentParticipants(Random.nextInt(currentParticipants.size))
    val newOptionalState = currentOptionalState map { state =>
      ClosedState(state.raffleId, winner)
    }
    println(s"\nnew optional state = $newOptionalState")
    setNestedState(newOptionalState) flatMap { _ =>
      setState {
        val newHistory = currentHistory :+ WinnerSelectedEvent(winner, OffsetDateTime.now, getRaffleId(currentOptionalState))
        println(s"\nnew history = $newHistory")
        newHistory
      }
    }
  }

  override protected lazy val handleCreateRaffleCommand: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleCommand.type =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (createRaffleCondition(currentOptionalState)) {
            createRaffleBlock(currentHistory)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
  }

  override protected lazy val handleCreateRaffleAddingParticipantCommand: PartialRaffleCommandHandler[M] = {
    case command: CreateRaffleAddingParticipantCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (createRaffleAddingParticipantCondition(currentOptionalState)) {
            createRaffleAddingParticipantBlock(command.name, currentHistory, currentOptionalState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
  }

  override protected lazy val handleAddParticipantCommand: PartialRaffleCommandHandler[M] = {
    case command: AddParticipantCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (addParticipantCondition(command.name, currentOptionalState)) {
            addParticipantBlock(command.name, currentHistory, currentOptionalState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
  }

  override protected lazy val handleRemoveParticipantCommand: PartialRaffleCommandHandler[M] = {
    case command: RemoveParticipantCommand =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (removeParticipantCondition(command.name, currentOptionalState)) {
            removeParticipantBlock(command.name, currentHistory, currentOptionalState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
  }

  override protected lazy val handleSelectWinnerCommand: PartialRaffleCommandHandler[M] = {
    case command: SelectWinnerCommand.type =>
      println(s"\ncase $command =>")
      getState(()) flatMap { currentHistory =>
        println(s"\ncurrent history = $currentHistory")
        getNestedState(()) flatMap { currentOptionalState =>
          println(s"\ncurrent optional state = $currentOptionalState")
          if (selectWinnerCondition(currentOptionalState)) {
            selectWinnerBlock(currentHistory, currentOptionalState)
          } else {
            failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
          }
        }
      }
  }

}
