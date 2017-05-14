package io.purefuncqrses.behavior

import io.purefuncqrses.features.{FailureF, RunF, State1F, SuccessF}

import scala.collection.immutable

import scala.language.higherKinds

object Behavior {

  type History[+E] = immutable.Seq[E]

  type PartialHandler[C, M[+ _]] = PartialFunction[C, M[Unit]]

  type PartialHandlers[C, M[+ _]] = List[PartialHandler[C, M]]

  type Handler[C, M[+ _]] = Function[C, M[Unit]]

  type HandlerForAll[C, M[+ _]] = Function[immutable.Seq[C], M[Unit]]

  def empty[E] = immutable.Seq[E]()

  def seq[C](cs: C*) = immutable.Seq[C](cs: _*)

}

import Behavior._

abstract class Behavior[C, E, I, M[+ _] : SuccessF : FailureF : State1F[History[E], ?[_]]] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  protected val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  def handlerForAll: HandlerForAll[C, M] =
    forAll[C](_)(handler)

  protected val partialHandlers: PartialHandlers[C, M]

  private val unknownHandler: PartialHandler[C, M] = {
    case c =>
      println(s"\ncase $c =>")
      failure(new IllegalStateException(s"unknown $c"))
  }

  private def handler: Handler[C, M] =
    partialHandlers.foldRight(unknownHandler)(_ orElse _)

}
