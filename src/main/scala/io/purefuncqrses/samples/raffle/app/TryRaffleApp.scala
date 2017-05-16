package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.behavior.State

import scala.util.Try

object TryRaffleApp extends App {

  new RaffleApp[λ[`+A` => StateTransformed[State, Try, A]]].runApp()

}
