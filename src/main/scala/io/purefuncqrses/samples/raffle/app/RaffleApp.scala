package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior._

import scala.language.higherKinds

class RaffleApp[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]] : RunF]
  extends AbstractRaffleApp[M] {

  import implicitRunF._


  override protected val raffleBehavior = new StatelessRaffleBehavior[M]

//  override protected val raffleBehavior: Behavior[RaffleCommand, RaffleEvent, RaffleId, M] = new StatefulRaffleBehavior[M]

  override protected val input = (HistoryArg(empty), ()).asInstanceOf[Input]


}

