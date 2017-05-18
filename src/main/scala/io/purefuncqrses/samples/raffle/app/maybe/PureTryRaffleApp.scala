package io.purefuncqrses.samples.raffle.app.maybe

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{HistoryState, RaffleState}

import scala.util.Try

object PureTryRaffleApp extends App {

  implicit val tryRaffleStateF = tryStateF[HistoryState]

  new PureRaffleApp[λ[`+A` => StateTransformed[HistoryState, Try, A]]].runApp()

}
