package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.behavior.State

import scala.concurrent.Future

object FutureRaffleApp extends App {

  new HistoryArgRaffleApp[λ[`+A` => StateTransformed[State, Future, A]]].runApp()

}
