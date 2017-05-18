package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.features._

import scala.language.higherKinds

class PureOptionalStateRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[RaffleHistoryAndOptionalRaffleAggregateState, ?[_]]]
  extends OptimizedRaffleBehavior[RaffleHistoryAndOptionalRaffleAggregateState, M] {

}
