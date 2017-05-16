package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.State

import scala.util.Try

object TryRaffleApp extends App {

  implicit val tryRaffleStateF = tryStateF[State]

  new RaffleApp[λ[`+A` => State => Try[(State, A)]]].runApp()

}
