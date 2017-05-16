package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.{History, empty}
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior._
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class PureRaffleApp[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]] : RunF]
  extends AbstractRaffleApp[M] {

  import implicitRunF._

  override protected val raffleBehavior: AbstractRaffleBehavior[M] = new PureStatefulRaffleBehavior[M]

  override protected val input: Input = (HistoryAndOptionalStateArgs(empty, None), ()).asInstanceOf[Input]

}

