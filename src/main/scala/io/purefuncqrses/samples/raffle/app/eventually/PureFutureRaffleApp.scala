package io.purefuncqrses.samples.raffle.app.eventually

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{HistoryState, RaffleState}

import scala.concurrent.Future

object PureFutureRaffleApp extends App {

  implicit val futureRaffleStateF = futureStateF[HistoryState]

  new PureRaffleApp[λ[`+A` => StateTransformed[HistoryState, Future, A]]].runApp()

}
