package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.behavior.HistoryAndOptionalAggregateArgs
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class PureOptionalStateRaffleApp[M[+ _] : SuccessF : FailureF : StateF[RaffleHistoryAndOptionalRaffleAggregateState, ?[_]] : RunF]
  extends RaffleApp[RaffleHistoryAndOptionalRaffleAggregateArgs, RaffleHistoryAndOptionalRaffleAggregateState, M] {

  import implicitRunF._

  override protected val raffleBehavior = new PureOptionalStateRaffleBehavior[M]

  override protected val input = (HistoryAndOptionalAggregateArgs[RaffleEvent, RaffleAggregate](empty, None), ()).asInstanceOf[Input]

}

