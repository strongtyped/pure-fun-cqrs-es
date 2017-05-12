package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.Identity
import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.RaffleState
import io.purefuncqrses.samples.raffle.behavior.StatelessRaffleBehavior.RaffleHistory

object PureIdentityIdentityRaffleApp extends App {

  implicit val identityOptionalRaffleStateStateF = identityStateF[Option[RaffleState]]

  implicit val identityIdentityRaffleHistoryNestedStateF = identityIdentityNestedStateF[RaffleHistory, Option[RaffleState]]

  new PureRaffleApp[λ[`+A` => RaffleHistory => Option[RaffleState] => Identity[(Option[RaffleState], (RaffleHistory, A))]]]

}
