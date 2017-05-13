package io.purefuncqrses.features.transform

import io.purefuncqrses.features._

import scala.language.higherKinds

class StateT[S, M[+ _] : SuccessF : FailureF : RunF]
  extends SuccessF[λ[`+A` => S => M[(S, A)]]]
    with FailureF[λ[`+A` => S => M[(S, A)]]]
    with RunF[λ[`+A` => S => M[(S, A)]]]
    with StateF[S, λ[`+A` => S => M[(S, A)]]] {

  val implicitSuccessF: SuccessF[M] = implicitly[SuccessF[M]]
  val implicitFailureF: FailureF[M] = implicitly[FailureF[M]]
  val implicitRunF: RunF[M] = implicitly[RunF[M]]

  override def success[A](a: A): S => M[(S, A)] =
    s =>
      implicitSuccessF.success((s, a))

  override def flatMap[A, B](f_s2msa: S => M[(S, A)])(f_a2f_s2msa: A => S => M[(S, B)]): S => M[(S, B)] =
    s =>
      implicitSuccessF.flatMap(f_s2msa(s)) {
        case (s, a) =>
          f_a2f_s2msa(a)(s)
      }

  override def failure[A](throwable: Throwable): S => M[(S, A)] =
    s =>
      implicitFailureF.failure(throwable)

  override def attempt[A](a: => A): S => M[(S, A)] =
    s =>
      implicitFailureF.attempt((s, a))

  override def recover[A](fa: S => M[(S, A)])(pf_t2f_s2msa: PartialFunction[Throwable, S => M[(S, A)]]): S => M[(S, A)] =
    s =>
      implicitFailureF.recover(fa(s)) {
        case throwable =>
          pf_t2f_s2msa(throwable)(s)
      }

  override val setState: S => S => M[(S, Unit)] =
    s =>
      _ =>
        implicitSuccessF.success((s, ()))

  override val getState: Unit => S => M[(S, S)] =
    _ =>
      s =>
        implicitSuccessF.success((s, s))

  override type Input = (S, implicitRunF.Input)

  override def run[A, Output](f_s2msa: S => M[(S, A)]): Input => Output = {
    case (s, i) =>
      implicitRunF.run(f_s2msa(s))(i).asInstanceOf[Output]
  }

}

class NestedStateT[S, T, M[+ _] : SuccessF : FailureF : StateF[T, ?[_]] : RunF]
  extends StateT[S, M]
    with NestedStateF[T, λ[`+A` => S => M[(S, A)]]] {

  val implicitStateF: StateF[T, M] = implicitly[StateF[T, M]]

  def nest[Z, Y](f: Z => M[Y]): Z => S => M[(S, Y)] =
  z =>
    s =>
      implicitSuccessF.map(f(z))((s, _))

  override val setNestedState: T => S => M[(S, Unit)] =
    nest(implicitStateF.setState)

  override val getNestedState: Unit => S => M[(S, T)] =
    nest(implicitStateF.getState)

}
