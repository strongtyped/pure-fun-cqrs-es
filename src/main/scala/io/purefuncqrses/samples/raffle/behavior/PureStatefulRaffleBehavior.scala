package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.features._
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.HandlerBody

import scala.language.higherKinds

class PureStatefulRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[State, ?[_]]]
  extends AbstractOptimizedRaffleBehavior[M] {

  import implicitFailureF._

  import implicitStateF._


  override protected def setState(args: Args): M[Unit] = args match {
    case state: HistoryAndOptionalStateArgs =>
      write {
        state
      }
    case _ =>
      failure(new IllegalStateException(s"$args are not 'history and optional state' arguments"))
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      case state: HistoryAndOptionalStateArgs =>
        handlerBody(command, state)
      case state =>
        failure(new IllegalStateException(s"$state is not a 'history and optional state' state"))
    }
  }

}
