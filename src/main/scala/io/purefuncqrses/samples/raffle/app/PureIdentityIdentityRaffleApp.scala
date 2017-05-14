package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.Identity
import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.RaffleState

object PureIdentityIdentityRaffleApp extends App {

  implicit val identityRaffleHistoryStateF = identityState1F[RaffleHistory]

  implicit val identityOptionalRaffleStateStateF = identityState1F[Option[RaffleState]]

  implicit val identityRaffleHistoryOptionalRaffleStateNestF = identityNestStateF[RaffleHistory, Option[RaffleState]]

  implicit val identityIdentityRaffleHistoryNestedStateF = identityIdentityNestedState2F[RaffleHistory, Option[RaffleState]]

  new PureRaffleApp[λ[`+A` => RaffleHistory => Option[RaffleState] => Identity[(Option[RaffleState], (RaffleHistory, A))]]].runApp()

}
