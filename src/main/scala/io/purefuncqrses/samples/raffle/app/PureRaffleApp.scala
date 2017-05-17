package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior._

import scala.language.higherKinds

class PureRaffleApp[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]] : RunF]
  extends RaffleApp[M] {

  import implicitRunF._


  override protected val raffleBehavior = new PureRaffleBehavior[M]

  override protected val input = (HistoryArg(empty), ()).asInstanceOf[Input]


}

