package io.purefuncqrses.behavior

import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}

import scala.collection.immutable

import scala.language.higherKinds

object Behavior {

  type All[+C] = immutable.Seq[C]

  def seq[C](cs: C*) = immutable.Seq[C](cs: _*)

  type History[+E] = immutable.Seq[E]

  def empty[E] = immutable.Seq[E]()

  type PartialHandler[C, M[+ _]] = PartialFunction[C, M[Unit]]

  type Handler[C, M[+ _]] = Function[All[C], M[Unit]]

}

import Behavior._

abstract class Behavior[C, E, I, M[+ _] : SuccessF : FailureF : StateF[History[E], ?[_]]] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  protected val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  protected val commandHandlers: List[PartialHandler[C, M]]

  private def handleUnknown: PartialHandler[C, M] = {
    case c =>
      println(s"\ncase $c =>")
      failure(new IllegalStateException(s"unknown $c"))
  }

  def handle: Handler[C, M] =
    doForAll[C](commandHandlers.foldRight(handleUnknown)(_ orElse _))

}
