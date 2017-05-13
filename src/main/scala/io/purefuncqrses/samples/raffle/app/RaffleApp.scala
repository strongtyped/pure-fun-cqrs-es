package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.{History, empty}
import io.purefuncqrses.features.{FailureF, RunF, State1F, SuccessF}
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.{RaffleHistory}
import io.purefuncqrses.samples.raffle.behavior.{AbstractRaffleBehavior, StatefulRaffleBehavior, StatelessRaffleBehavior}
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

class RaffleApp[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]] : RunF]
  extends AbstractRaffleApp[M] {

  import implicitRunF._

//  override protected val raffleBehavior: AbstractRaffleBehavior[M] = new StatelessRaffleBehavior[M]

  override protected val raffleBehavior: AbstractRaffleBehavior[M] = new StatefulRaffleBehavior[M]

  override protected val input: Input = (empty, ()).asInstanceOf[Input]

  override protected type Output = (History[RaffleEvent], Unit)

  override protected val raffleHistory: RaffleHistory = output()._1

}

