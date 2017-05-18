package io.purefuncqrses.samples.raffle.app.maybe

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.OptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryState, RaffleState}

import scala.util.Try

object OptionalStateTryRaffleApp extends App {

  implicit val tryRaffleStateF = tryStateF[RaffleHistoryState]

  new OptionalStateRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryState, Try, A]]].runApp()

}
