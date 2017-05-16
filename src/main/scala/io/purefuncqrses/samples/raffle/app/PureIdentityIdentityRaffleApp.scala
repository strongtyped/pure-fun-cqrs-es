package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.Identity
import io.purefuncqrses.features.implicits.FeaturesImplicits._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.{RaffleState, State}

import scala.util.Try

object PureIdentityIdentityRaffleApp extends App {

  implicit val identityRaffleStateF = identityStateF[State]

  // implicit val identityOptionalRaffleStateStateF = identityStateF[Option[RaffleState]]

  // implicit val identityRaffleHistoryOptionalRaffleStateNestF = identityNestStateF[State, Option[RaffleState]]

  // implicit val identityIdentityRaffleHistoryNestedStateF = identityIdentityNestedState2F[RaffleHistory, Option[RaffleState]]

  new PureRaffleApp[λ[`+A` => State => Try[(State, A)]]].runApp()

}
