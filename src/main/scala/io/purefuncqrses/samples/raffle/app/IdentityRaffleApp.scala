package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.Identity
import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.State

object IdentityRaffleApp extends App {

  implicit val identityRaffleStateF = identityStateF[State]

  new RaffleApp[λ[`+A` => State => Identity[(State, A)]]].runApp()

}
