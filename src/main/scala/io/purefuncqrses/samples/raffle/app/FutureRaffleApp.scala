package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.State
import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.Args

import scala.concurrent.Future

object FutureRaffleApp extends App {

  implicit val futureRaffleHistoryStateF = futureState1F[State]

  new RaffleApp[λ[`+A` => Args => Future[(Args, A)]]].runApp()

}
