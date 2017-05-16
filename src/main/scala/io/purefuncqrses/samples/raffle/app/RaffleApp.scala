package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior
import io.purefuncqrses.behavior.Behavior.{State, empty}
import io.purefuncqrses.features.{FailureF, RunF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior._
import io.purefuncqrses.samples.raffle.commands.RaffleCommand
import io.purefuncqrses.samples.raffle.events.RaffleEvent
import io.purefuncqrses.samples.raffle.id.RaffleId

import scala.language.higherKinds

class RaffleApp[M[+ _] : SuccessF : FailureF : State1F[State, ?[_]] : RunF]
  extends AbstractRaffleApp[M] {

  import implicitRunF._

//  override protected val raffleBehavior: AbstractRaffleBehavior[M] = new StatelessRaffleBehavior[M]

  override protected val raffleBehavior: Behavior[RaffleCommand, RaffleEvent, RaffleId, M] = new StatefulRaffleBehavior[M]

  override protected val input: Input = (History_Arg(empty), ()).asInstanceOf[Input]

}

