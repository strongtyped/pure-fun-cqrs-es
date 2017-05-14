package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory

import scala.concurrent.Future

object FutureRaffleApp extends App {

  implicit val futureRaffleHistoryStateF = futureState1F[RaffleHistory]

  new RaffleApp[λ[`+A` => RaffleHistory => Future[(RaffleHistory, A)]]].runApp()

}
