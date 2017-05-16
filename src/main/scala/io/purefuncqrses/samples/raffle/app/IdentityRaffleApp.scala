package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.Identity
import io.purefuncqrses.behavior.Behavior.State
import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.Args

object IdentityRaffleApp extends App {

  implicit val identityRaffleHistoryStateF = identityState1F[State]

  new RaffleApp[λ[`+A` => Args => Identity[(Args, A)]]].runApp()

}
