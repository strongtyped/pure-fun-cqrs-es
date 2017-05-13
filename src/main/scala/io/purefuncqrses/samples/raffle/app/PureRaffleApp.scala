package io.purefuncqrses.samples.raffle.app

import io.purefuncqrses.behavior.Behavior.{History, empty}
import io.purefuncqrses.features._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.{AbstractRaffleBehavior, PureStatefulRaffleBehavior, RaffleState}
import io.purefuncqrses.samples.raffle.events.RaffleEvent

import scala.language.higherKinds

 class PureRaffleApp[M[+ _] : SuccessF : FailureF : State1F[RaffleHistory, ?[_]] : State2F[Option[RaffleState], ?[_]] : RunF]
  extends AbstractRaffleApp[M] {

  import implicitRunF._

  override protected val raffleBehavior: AbstractRaffleBehavior[M] = new PureStatefulRaffleBehavior[M]

  override protected val input: Input = (empty, (None, ())).asInstanceOf[Input]

  override protected type Output = (Option[RaffleState], (History[RaffleEvent], Unit))

  override protected val raffleHistory: RaffleHistory = output()._2._1

}

