package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.Identity
import io.purefuncqrses.samples.raffle.behavior.StatelessRaffleBehavior.RaffleHistory
import io.purefuncqrses.features.implicits.FeaturesImplicits._

object IdentityRaffleApp extends App {

  implicit val identityRaffleHistoryStateF = identityStateF[RaffleHistory]

  new RaffleApp[λ[`+A` => RaffleHistory => Identity[(RaffleHistory, A)]]]

}
