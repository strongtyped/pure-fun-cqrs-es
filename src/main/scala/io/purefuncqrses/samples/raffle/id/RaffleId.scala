package io.purefuncqrses.samples.raffle.id

import java.util.UUID

import com.typesafe.scalalogging.Logger

case class RaffleId(value: String)

object RaffleId {

  val logger = Logger("RaffleId")

  def generate(): RaffleId =
    RaffleId(UUID.randomUUID().toString)

}

