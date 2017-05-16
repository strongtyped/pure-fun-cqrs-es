package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.State

import scala.concurrent.Future

object FutureRaffleApp extends App {

  implicit val futureRaffleStateF = futureStateF[State]

  new RaffleApp[λ[`+A` => State => Future[(State, A)]]].runApp()

}
