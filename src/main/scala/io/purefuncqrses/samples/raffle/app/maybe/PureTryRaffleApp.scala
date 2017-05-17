package io.purefuncqrses.samples.raffle.app.maybe

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureRaffleApp
import io.purefuncqrses.samples.raffle.behavior.State

import scala.util.Try

object PureTryRaffleApp extends App {

  implicit val tryRaffleStateF = tryStateF[State]

  new PureRaffleApp[λ[`+A` => StateTransformed[State, Try, A]]].runApp()

}
