package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.Identity
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.behavior.State


object PureIdentityRaffleApp extends App {

  new PureRaffleApp[λ[`+A` => StateTransformed[State, Identity, A]]].runApp()

}
