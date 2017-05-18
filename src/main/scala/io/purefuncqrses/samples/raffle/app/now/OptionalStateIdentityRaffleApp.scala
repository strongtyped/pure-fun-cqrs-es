package io.purefuncqrses.samples.raffle.app.now

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.OptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryState, RaffleState}
import io.purefuncqrses.Identity

object OptionalStateIdentityRaffleApp extends App {

  implicit val identityRaffleStateF = identityStateF[RaffleHistoryState]

  new OptionalStateRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryState, Identity, A]]].runApp()

}
