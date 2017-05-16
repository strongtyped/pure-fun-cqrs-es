package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior._

import scala.language.higherKinds

class PureRaffleApp[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]] : RunF]
  extends AbstractRaffleApp[M] {

  import implicitRunF._

  override protected val raffleBehavior = new PureStatefulRaffleBehavior[M]

  override protected val input = (HistoryAndOptionalStateArgs(empty, None), ()).asInstanceOf[Input]

}

