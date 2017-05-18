package io.purefuncqrses.samples.raffle.app.now

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureOptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryAndOptionalRaffleAggregateState, RaffleState}
import io.purefuncqrses.Identity

object PureOptionalStateIdentityRaffleApp extends App {

  implicit val identityRaffleStateF = identityStateF[RaffleHistoryAndOptionalRaffleAggregateState]

  new PureOptionalStateRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryAndOptionalRaffleAggregateState, Identity, A]]].runApp()

}
