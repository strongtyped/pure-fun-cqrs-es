package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.behavior.HistoryArg
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class PureRaffleApp[M[+ _] : SuccessF : FailureF : StateF[HistoryState, ?[_]] : RunF]
  extends RaffleApp[RaffleHistoryArg, HistoryState, M] {

  import implicitRunF._


  override protected val raffleBehavior = new PureRaffleBehavior[M]

  override protected val input = (HistoryArg[RaffleEvent, RaffleState](empty), ()).asInstanceOf[Input]


}

