package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.behavior.HistoryAndOptionalAggregateStateArgs
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class PureOptionalStateRaffleApp[M[+ _] : SuccessF : FailureF : StateF[HistoryAndOptionalStateState, ?[_]] : RunF]
  extends RaffleApp[RaffleHistoryAndOptionalRaffleStateArgs, HistoryAndOptionalStateState, M] {

  import implicitRunF._

  override protected val raffleBehavior = new PureOptionalStateRaffleBehavior[M]

  override protected val input = (HistoryAndOptionalAggregateStateArgs[RaffleEvent, RaffleAggregateState](empty, None), ()).asInstanceOf[Input]

}

