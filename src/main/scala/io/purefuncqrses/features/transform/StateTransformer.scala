package io.purefuncqrses.features.transform

import io.purefuncqrses.features._

import scala.language.higherKinds

object StateTransformer {

  type StateTransformed[S, M[+ _], +A] = S => M[(S, A)]

}

import StateTransformer._

class StateTransformer[S, M[+ _] : SuccessF : FailureF : RunF]
  extends SuccessF[λ[`+A` => StateTransformed[S, M, A]]]
    with FailureF[λ[`+A` => StateTransformed[S, M, A]]]
    with RunF[λ[`+A` => StateTransformed[S, M, A]]]
    with StateF[S, λ[`+A` => StateTransformed[S, M, A]]] {

  val implicitSuccessF: SuccessF[M] = implicitly[SuccessF[M]]
  val implicitFailureF: FailureF[M] = implicitly[FailureF[M]]
  val implicitRunF: RunF[M] = implicitly[RunF[M]]

  override def success[A](a: A): StateTransformed[S, M, A] =
    s =>
      implicitSuccessF.success((s, a))

  override def flatMap[A, B](sma: StateTransformed[S, M, A])(f_a2smb: A => StateTransformed[S, M, B]): StateTransformed[S, M, B] =
    s =>
      implicitSuccessF.flatMap(sma(s)) {
        case (s, a) =>
          f_a2smb(a)(s)
      }

  override def failure[A](throwable: Throwable): StateTransformed[S, M, A] =
    s =>
      implicitFailureF.failure(throwable)

  override def attempt[A](a: => A): StateTransformed[S, M, A] =
    s =>
      implicitFailureF.attempt((s, a))

  override def recover[A](sma: StateTransformed[S, M, A])(pf_t2sma: PartialFunction[Throwable, StateTransformed[S, M, A]]): StateTransformed[S, M, A] =
    s =>
      implicitFailureF.recover(sma(s)) {
        case throwable =>
          pf_t2sma(throwable)(s)
      }

  override val write: S => StateTransformed[S, M, Unit] =
    s =>
      _ =>
        implicitSuccessF.success((s, ()))

  override val read: Unit => StateTransformed[S, M, S] =
    _ =>
      s =>
        implicitSuccessF.success((s, s))

  override type Input = (S, implicitRunF.Input)

  override def run[A, Output](sma: StateTransformed[S, M, A]): Input => Output = {
    case (s, i) =>
      implicitRunF.run(sma(s))(i).asInstanceOf[Output]
  }

}


