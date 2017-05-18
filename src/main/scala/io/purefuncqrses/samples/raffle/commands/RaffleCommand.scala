package io.purefuncqrses.samples.raffle.commands

sealed trait RaffleCommand


case object CreateRaffleCommand extends RaffleCommand

case class CreateRaffleAddingParticipantCommand(name: String) extends RaffleCommand


case class AddParticipantCommand(name: String) extends RaffleCommand

case class RemoveParticipantCommand(name: String) extends RaffleCommand


case object SelectWinnerCommand extends RaffleCommand
