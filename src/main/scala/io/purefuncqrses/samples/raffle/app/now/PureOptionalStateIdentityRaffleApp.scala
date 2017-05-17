package io.purefuncqrses.samples.raffle.app.now

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureOptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.State

import io.purefuncqrses.Identity

object PureOptionalStateIdentityRaffleApp extends App {

  implicit val identityRaffleStateF = identityStateF[State]

  new PureOptionalStateRaffleApp[λ[`+A` => StateTransformed[State, Identity, A]]].runApp()

}
