package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.{Handler, History, PartialHandler}
import io.purefuncqrses.features.{FailureF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.commands.RaffleCommand
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.collection.immutable
import scala.language.higherKinds

object AbstractRaffleBehavior {
  type RaffleCommands = immutable.Seq[RaffleCommand]
  type RaffleHistory = History[RaffleEvent]
  type PartialRaffleCommandHandler[M[+ _]] = PartialHandler[RaffleCommand, M]
  type PartialRaffleCommandHandlers[M[+ _]] = List[PartialRaffleCommandHandler[M]]
  type RaffleCommandHandler[M[+ _]] = Handler[RaffleCommand, M]
}

import AbstractRaffleBehavior._

abstract class AbstractRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistory, ?[_]]]
  extends Behavior[RaffleCommand, RaffleEvent, RaffleId, M] {

  protected def handleCreateRaffleCommand: PartialRaffleCommandHandler[M]

  protected def handleCreateRaffleAddingParticipantCommand: PartialRaffleCommandHandler[M]

  protected def handleAddParticipantCommand: PartialRaffleCommandHandler[M]

  protected def handleRemoveParticipantCommand: PartialRaffleCommandHandler[M]

  protected def handleSelectWinnerCommand: PartialRaffleCommandHandler[M]

  override protected val partialHandlers: PartialRaffleCommandHandlers[M] =
    List(
      handleCreateRaffleCommand,
      handleCreateRaffleAddingParticipantCommand,
      handleAddParticipantCommand,
      handleRemoveParticipantCommand,
      handleSelectWinnerCommand
    )

}


