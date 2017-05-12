package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.StatelessRaffleBehavior.RaffleHistory

import scala.concurrent.Future

object FutureRaffleApp extends App {

  implicit val futureRaffleHistoryStateF = futureStateF[RaffleHistory]

  new RaffleApp[λ[`+A` => RaffleHistory => Future[(RaffleHistory, A)]]]

}
