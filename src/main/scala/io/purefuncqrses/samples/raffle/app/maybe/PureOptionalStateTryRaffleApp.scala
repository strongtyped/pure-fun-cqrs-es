package io.purefuncqrses.samples.raffle.app.maybe

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureOptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryAndOptionalRaffleAggregateState, RaffleState}

import scala.util.Try

object PureOptionalStateTryRaffleApp extends App {

  implicit val tryRaffleStateF = tryStateF[RaffleHistoryAndOptionalRaffleAggregateState]

  new PureOptionalStateRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryAndOptionalRaffleAggregateState, Try, A]]].runApp()

}
