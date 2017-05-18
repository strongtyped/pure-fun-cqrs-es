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

  type HandlerBody[A, C, M[+ _]] = (C, A) => M[Unit]


  type HandleAll[C, M[+ _]] = immutable.Seq[C] => M[Unit]


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

  // pure default: A = S
  // override for impure state (A != S)
  protected def setStateFromArgs(args: A): M[Unit] =
  write {
    args.asInstanceOf[S]
  }

  // pure default: A = S
  // override for impure state (A != S)
  protected def handlerTemplate[Cmd <: C](condition: A => Boolean, newArgs: A => A): Handler[Cmd, M] = { command =>
    read(()) flatMap { state =>
      val args: A = state.asInstanceOf[A]
      val currentHistory: History[E] = args.getHistory
      println(s"\ncurrent history = $currentHistory")
      if (condition(args)) {
        setStateFromArgs(newArgs(args))
      } else {
        failure(new IllegalStateException(s"$command not applicable with history $currentHistory"))
      }
    }
  }

  // do not override (not possible anyway)
  protected final def newHistoryFor(i: I, args: A, es: E*): History[E] = {
    val newHistory: History[E] = es.foldLeft(args.getHistory)(_ :+ _)
    println(s"new history = $newHistory")
    newHistory
  }

  // do not override (not possible anyway)
  protected final def newHistoryFor(args: A, es: E*): History[E] = {
    val newHistory: History[E] = es.foldLeft(args.getHistory)(_ :+ _)
    println(s"new history = $newHistory")
    newHistory
  }

  // define
  protected val partialHandlers: PartialHandlers[C, M]

  private val failurePartialHandler: PartialHandler[C, M] = {
    case c =>
      println(s"\ncase $c =>")
      failure(new IllegalStateException(s"unknown $c"))
  }

  def handleAll: HandleAll[C, M] =
    traverse(partialHandlers.foldRight(failurePartialHandler)(_ orElse _))(_).ignore

}
