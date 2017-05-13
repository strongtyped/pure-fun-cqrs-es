package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory

import scala.util.Try

object TryRaffleApp extends App {

  implicit val tryRaffleHistoryStateF = tryStateF[RaffleHistory]

  new RaffleApp[λ[`+A` => RaffleHistory => Try[(RaffleHistory, A)]]]

}
