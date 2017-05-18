package io.purefuncqrses.samples.raffle.app.maybe

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryState, RaffleState}

import scala.util.Try

object PureTryRaffleApp extends App {

  implicit val tryRaffleStateF = tryStateF[RaffleHistoryState]

  new PureRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryState, Try, A]]].runApp()

}
