package io.purefuncqrses.samples.raffle.app.eventually

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.OptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{HistoryState, State}

import scala.concurrent.Future

object OptionalStateFutureRaffleApp extends App {

  implicit val futureRaffleStateF = futureStateF[HistoryState]

  new OptionalStateRaffleApp[λ[`+A` => StateTransformed[HistoryState, Future, A]]].runApp()

}
