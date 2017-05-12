package io.purefuncqrses.behavior

import io.purefuncqrses.features.{FailureF, RunF, StateF, SuccessF}

import scala.collection.immutable

import scala.language.higherKinds

object Behavior {
  type All[+C] = immutable.Seq[C]

  def all[C](cs: C*) = immutable.Seq[C](cs: _*)

  type History[+E] = immutable.Seq[E]

  def empty[E] = immutable.Seq[E]()
}

import Behavior._

abstract class Behavior[C, E, I, M[+ _] : SuccessF : FailureF : StateF[History[E], ?[_]]] {

  private val implicitSuccessF = implicitly[SuccessF[M]]

  import implicitSuccessF._

  private val implicitFailureF = implicitly[FailureF[M]]

  import implicitFailureF._

  private val implicitStateF = implicitly[StateF[History[E], M]]

  import implicitStateF._

  protected def handle: C => M[Unit]

  def handleAll: All[C] => M[Unit] =
    doAll[C](handle)

}
