package io.purefuncqrses.samples.raffle.app.now

import io.purefuncqrses.Identity
import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{RaffleHistoryState, RaffleState}

object PureIdentityRaffleApp extends App {

  implicit val identityRaffleStateF = identityStateF[RaffleHistoryState]

  new PureRaffleApp[λ[`+A` => StateTransformed[RaffleHistoryState, Identity, A]]].runApp()

}
