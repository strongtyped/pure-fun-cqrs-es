package io.purefuncqrses.behavior

import io.purefuncqrses.features.{SuccessF, FailureF, StateF}
import io.purefuncqrses.features.ops.FeatureOps._
import io.purefuncqrses.samples.raffle.behavior.State

import scala.collection.immutable
import scala.language.higherKinds

object Behavior {

  type History[+E] = immutable.Seq[E]

  type PartialHandler[C, M[+ _]] = PartialFunction[C, M[Unit]]

  type PartialHandlers[C, M[+ _]] = List[PartialHandler[C, M]]

  type Handler[C, M[+ _]] = C => M[Unit]

  type HandlerBody[A, C, M[+ _]] = (C, A) => M[Unit]

  type HandlerForAll[C, M[+ _]] = immutable.Seq[C] => M[Unit]


  def empty[E] = immutable.Seq[E]()

  def seq[C](cs: C*) = immutable.Seq[C](cs: _*)

}

import Behavior._

abstract class Behavior[A <: HasHistory[E], S, C, E, I, M[+ _] : SuccessF : FailureF : StateF[S, ?[_]]] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  protected val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  protected val implicitStateF = implicitly[StateF[S, M]]

  import implicitStateF._


  protected def setStateFromArgs(args: A): M[Unit] = {
    val state: S = args.asInstanceOf[S] // default A = S
    write {
      state
    }
  }


  protected def handlerTemplate[Cmd](condition: A => Boolean, newArgs: A => A): Handler[Cmd, M] = { command =>
    read(()) flatMap {
      case state =>
        val args: A = state.asInstanceOf[A] // default A = S
      val currentHistory: History[E] = args.getHistory
        println(s"\ncurrent history = $currentHistory")
        if (condition(args)) {
          setStateFromArgs(newArgs(args))
        } else {
          failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
        }
    }
  }


  protected val partialHandlers: PartialHandlers[C, M]

  private val failurePartialHandler: PartialHandler[C, M] = {
    case c =>
      println(s"\ncase $c =>")
      failure(new IllegalStateException(s"unknown $c"))
  }

  private def handle: Handler[C, M] =
    partialHandlers.foldRight(failurePartialHandler)(_ orElse _)


  def handleAll: HandlerForAll[C, M] =
    traverse(handle)(_).ignore

}
