package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.behavior.State

import io.purefuncqrses.Identity

object IdentityRaffleApp extends App {

  new RaffleApp[λ[`+A` => StateTransformed[State, Identity, A]]].runApp()

}
