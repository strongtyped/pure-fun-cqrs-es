package io.purefuncqrses.samples.raffle.app.eventually

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureOptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryAndOptionalRaffleAggregateState, RaffleHistoryState, RaffleState}

import scala.concurrent.Future

object PureOptionalStateFutureRaffleApp extends App {

  implicit val futureRaffleStateF = futureStateF[RaffleHistoryAndOptionalRaffleAggregateState]

  new PureOptionalStateRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryAndOptionalRaffleAggregateState, Future, A]]].runApp()

}
