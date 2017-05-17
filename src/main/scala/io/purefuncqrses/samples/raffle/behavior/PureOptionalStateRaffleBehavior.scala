package io.purefuncqrses.samples.raffle.behavior

import io.purefuncqrses.behavior.Behavior.Handler
import io.purefuncqrses.features._
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.RaffleBehavior.HandlerBody

import scala.language.higherKinds

class PureOptionalStateRaffleBehavior[M[+ _] : SuccessF : FailureF : StateF[HistoryAndOptionalStateState, ?[_]]]
  extends OptimizedRaffleBehavior[HistoryAndOptionalStateState, M] {

  import implicitStateF._


  override protected def setState(args: HistoryAndOptionalStateArgs): M[Unit] = {
    val newState = args
    write {
      newState
    }
  }


  override protected def handlerTemplate[Cmd](handlerBody: HandlerBody[HistoryAndOptionalStateArgs, Cmd, M]): Handler[Cmd, M] = command => {
    println(s"\ncase $command =>")
    read(()) flatMap {
      case state =>
        handlerBody(command, state)
    }
  }

}
