package io.purefuncqrses.samples.raffle.app.eventually

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryState, RaffleState}

import scala.concurrent.Future

object PureFutureRaffleApp extends App {

  implicit val futureRaffleStateF = futureStateF[RaffleHistoryState]

  new PureRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryState, Future, A]]].runApp()

}
