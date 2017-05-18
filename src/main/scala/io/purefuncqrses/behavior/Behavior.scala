package io.purefuncqrses.behavior

import io.purefuncqrses.features.{SuccessF, FailureF, StateF}
import io.purefuncqrses.features.ops.FeatureOps._

import scala.collection.immutable
import scala.language.higherKinds

object Behavior {

  type History[+E] = immutable.Seq[E]

  type PartialHandler[C, M[+ _]] = PartialFunction[C, M[Unit]]

  type PartialHandlers[C, M[+ _]] = List[PartialHandler[C, M]]


  type Handler[C, M[+ _]] = C => M[Unit]

  //  type HandlerBody[A, C, M[+ _]] = (C, A) => M[Unit]

  type HandlerBlock[A, M[+ _]] = A => M[Unit]


  type HandleAll[C, M[+ _]] = immutable.Seq[C] => M[Unit]


  def empty[E] = immutable.Seq[E]()

  def seq[C](cs: C*) = immutable.Seq[C](cs: _*)

}

import Behavior._

/**
  *
  * @tparam A
  * arguments available for business logic (should, at least, have history)
  * @tparam S
  * state available for business logic (should, at least, have history)
  * A = S for a pure implementation
  * @tparam C
  * command
  * @tparam E
  * event
  * @tparam I
  * id
  * @tparam M
  * monad
  */

abstract class Behavior[A <: HasHistory[E], S, C, E, I, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  protected val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  protected val implicitStateF = implicitly[StateF[S, M]]

  import implicitStateF._

  // pure default: A = S
  // override for impure state (A != S)

  protected def setStateFromArgs(args: A): M[Unit] =
    write {
      args.asInstanceOf[S]
    }

  // pure default: A = S
  // override for impure state (A != S)

  protected def handlerTemplate[Cmd <: C](condition: A => Boolean, block: A => M[Unit]): Handler[Cmd, M] = { command =>
    read(()) flatMap { state =>
      val args: A = state.asInstanceOf[A]
      if (condition(args)) {
        block(args)
      } else {
        failure(new IllegalStateException(s"$command not applicable with history ${args.getHistory}"))
      }
    }
  }

  // do not override (not possible anyway)

  protected final def blockTemplate(transformer: A => A): HandlerBlock[A, M] = args => {
    val newArgs = transformer.apply(args)
    println(s"\n$newArgs")
    setStateFromArgs(newArgs)
  }

  protected final def updatedHistory(args: A, es: E*): History[E] =
    es.foldLeft(args.getHistory)(_ :+ _)

  // this is what you should define
  protected val partialHandlers: PartialHandlers[C, M]

  private val failurePartialHandler: PartialHandler[C, M] = {
    case command =>
      failure(new IllegalStateException(s"unknown command $command"))
  }

  // this is what you should use

  def handleAll: HandleAll[C, M] =
    traverse(partialHandlers.foldRight(failurePartialHandler)(_ orElse _))(_).ignore

}
