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

  type HandlerForAll[C, M[+ _]] = immutable.Seq[C] => M[Unit]


  def empty[E] = immutable.Seq[E]()

  def seq[C](cs: C*) = immutable.Seq[C](cs: _*)

}

import Behavior._

abstract class Behavior[C, E, I, M[+ _] : SuccessF : FailureF : StateF[State, ?[_]]] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  protected val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

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
