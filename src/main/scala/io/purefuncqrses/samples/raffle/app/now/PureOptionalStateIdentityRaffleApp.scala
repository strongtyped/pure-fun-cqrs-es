package io.purefuncqrses.samples.raffle.app.now

import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.features.transform.StateTransformer.StateTransformed
import io.purefuncqrses.samples.raffle.app.PureOptionalStateRaffleApp
import io.purefuncqrses.samples.raffle.behavior.{HistoryAndOptionalStateState, State}
import io.purefuncqrses.Identity

object PureOptionalStateIdentityRaffleApp extends App {

  implicit val identityRaffleStateF = identityStateF[HistoryAndOptionalStateState]

  new PureOptionalStateRaffleApp[λ[`+A` => StateTransformed[HistoryAndOptionalStateState, Identity, A]]].runApp()

}
