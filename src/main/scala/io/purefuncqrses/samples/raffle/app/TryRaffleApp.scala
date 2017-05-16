package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.Args

import scala.util.Try

object TryRaffleApp extends App {

  implicit val tryRaffleHistoryStateF = tryState2F[Args]

  new RaffleApp[λ[`+A` => Args => Try[(Args, A)]]].runApp()

}
