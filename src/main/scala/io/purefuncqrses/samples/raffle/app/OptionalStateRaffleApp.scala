package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.empty
import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}
import io.purefuncqrses.samples.raffle.behavior._
import io.purefuncqrses.samples.raffle.commands.RaffleCommand
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

class OptionalStateRaffleApp[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]] : RunF]
  extends RaffleApp[M] {

  import implicitRunF._


  override protected val raffleBehavior: Behavior[RaffleCommand, RaffleEvent, RaffleId, M] = new OptionalStateRaffleBehavior[M]

  override protected val input = (HistoryArg(empty), ()).asInstanceOf[Input]


}

