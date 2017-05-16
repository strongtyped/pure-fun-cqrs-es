package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.{State, empty}
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior._
import io.purefuncqrses.samples.raffle.commands.RaffleCommand
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

class PureRaffleApp[M[+ _] : SuccessF : FailureF : State1F[State, ?[_]] : RunF]
  extends AbstractRaffleApp[M] {

  import implicitRunF._

  override protected val raffleBehavior: Behavior[RaffleCommand, RaffleEvent, RaffleId, M] = new PureStatefulRaffleBehavior[M]

  override protected val input: Input = (History_And_OptionalState_Args(empty, None), ()).asInstanceOf[Input]

}

