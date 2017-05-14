package io.purefuncqrses.samples.raffle.commands

sealed trait RaffleCommand


case object CreateRaffleCommand extends RaffleCommand

case object SelectWinnerCommand extends RaffleCommand

trait RaffleCommandWithName extends RaffleCommand {
  val name : String
}

case class CreateRaffleAddingParticipantCommand(name: String) extends RaffleCommandWithName

case class AddParticipantCommand(name: String) extends RaffleCommandWithName

case class RemoveParticipantCommand(name: String) extends RaffleCommandWithName
