package io.purefuncqrses.samples.raffle.app.eventually

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureOptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.State

import scala.concurrent.Future

object PureOptionalStateFutureRaffleApp extends App {

  implicit val futureRaffleStateF = futureStateF[State]

  new PureOptionalStateRaffleApp[λ[`+A` => StateTransformed[State, Future, A]]].runApp()

}
