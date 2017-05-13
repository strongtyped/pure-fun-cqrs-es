package io.purefuncqrses.behavior

import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}

import scala.collection.immutable

import scala.language.higherKinds

object Behavior {

  def seq[C](cs: C*) = immutable.Seq[C](cs: _*)

  type History[+E] = immutable.Seq[E]

  def empty[E] = immutable.Seq[E]()

  type PartialHandler[C, M[+ _]] = PartialFunction[C, M[Unit]]

  type PartialHandlers[C, M[+ _]] = List[PartialHandler[C, M]]

  type Handler[C, M[+ _]] = Function[immutable.Seq[C], M[Unit]]

}

import Behavior._

abstract class Behavior[C, E, I, M[+ _] : SuccessF : FailureF : StateF[History[E], ?[_]]] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  protected val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  protected val partialHandlers: PartialHandlers[C, M]

  private def handleUnknown: PartialHandler[C, M] = {
    case c =>
      println(s"\ncase $c =>")
      failure(new IllegalStateException(s"unknown $c"))
  }

  def handle: Handler[C, M] =
    doForAll[C](partialHandlers.foldRight(handleUnknown)(_ orElse _))

}
