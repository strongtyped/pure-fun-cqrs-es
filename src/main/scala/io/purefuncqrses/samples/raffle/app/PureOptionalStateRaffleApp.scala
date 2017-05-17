package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior._

import scala.language.higherKinds

class PureOptionalStateRaffleApp[M[+ _] : SuccessF : FailureF : StateF[HistoryAndOptionalStateState, ?[_]] : RunF]
  extends RaffleApp[HistoryAndOptionalStateArgs, HistoryAndOptionalStateState, M] {

  import implicitRunF._

  override protected val raffleBehavior = new PureOptionalStateRaffleBehavior[M]

  override protected val input = (HistoryAndOptionalStateArgs(empty, None), ()).asInstanceOf[Input]

}

