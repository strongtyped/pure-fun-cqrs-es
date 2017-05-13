package io.purefuncqrses.features.transform

import io.purefuncqrses.features._

import scala.language.higherKinds

class State1T[S1, M[+ _] : SuccessF : FailureF : RunF]
  extends SuccessF[λ[`+A` => S1 => M[(S1, A)]]]
    with FailureF[λ[`+A` => S1 => M[(S1, A)]]]
    with RunF[λ[`+A` => S1 => M[(S1, A)]]]
    with State1F[S1, λ[`+A` => S1 => M[(S1, A)]]] {

  val implicitSuccessF: SuccessF[M] = implicitly[SuccessF[M]]
  val implicitFailureF: FailureF[M] = implicitly[FailureF[M]]
  val implicitRunF: RunF[M] = implicitly[RunF[M]]

  override def success[A](a: A): S1 => M[(S1, A)] =
    s =>
      implicitSuccessF.success((s, a))

  override def flatMap[A, B](f_s2msa: S1 => M[(S1, A)])(f_a2f_s2msa: A => S1 => M[(S1, B)]): S1 => M[(S1, B)] =
    s =>
      implicitSuccessF.flatMap(f_s2msa(s)) {
        case (s, a) =>
          f_a2f_s2msa(a)(s)
      }

  override def failure[A](throwable: Throwable): S1 => M[(S1, A)] =
    s =>
      implicitFailureF.failure(throwable)

  override def attempt[A](a: => A): S1 => M[(S1, A)] =
    s =>
      implicitFailureF.attempt((s, a))

  override def recover[A](fa: S1 => M[(S1, A)])(pf_t2f_s2msa: PartialFunction[Throwable, S1 => M[(S1, A)]]): S1 => M[(S1, A)] =
    s =>
      implicitFailureF.recover(fa(s)) {
        case throwable =>
          pf_t2f_s2msa(throwable)(s)
      }

  override val setState1: S1 => S1 => M[(S1, Unit)] =
    s =>
      _ =>
        implicitSuccessF.success((s, ()))

  override val getState1: Unit => S1 => M[(S1, S1)] =
    _ =>
      s =>
        implicitSuccessF.success((s, s))

  override type Input = (S1, implicitRunF.Input)

  override def run[A, Output](f_s2msa: S1 => M[(S1, A)]): Input => Output = {
    case (s, i) =>
      implicitRunF.run(f_s2msa(s))(i).asInstanceOf[Output]
  }

}

class State2T[S1, S2, M[+ _] : SuccessF : FailureF : State1F[S2, ?[_]] : NestF[S1, ?[_]] : RunF]
  extends State1T[S1, M]
    with State2F[S2, λ[`+A` => S1 => M[(S1, A)]]] {

  override val setState2: S2 => S1 => M[(S1, Unit)] =
    implicitly[NestF[S1, M]].nest(implicitly[State1F[S2, M]].setState1)

  override val getState2: Unit => S1 => M[(S1, S2)] =
    implicitly[NestF[S1, M]].nest(implicitly[State1F[S2, M]].getState1)

}


class State3T[S1, S2, S3, M[+ _] : SuccessF : FailureF : State1F[S2, ?[_]] : λ[`M[+ _]` => State2F[S3, λ[`+A` => S2 => M[(S2, A)]]]] : NestF[S1, ?[_]] : λ[`M[+ _]` => NestF[S1, λ[`+A` => S2 => M[(S2, A)]]]] : RunF]
  extends State2T[S1, S2, M]
    with State3F[S3, λ[`+A` => S1 => S2 => M[(S2, (S1, A))]]] {

  override val setState3: S3 => S1 => S2 => M[(S2, (S1, Unit))] =
    implicitly[NestF[S1, λ[`+A` => S2 => M[(S2, A)]]]].nest(implicitly[State2F[S3, λ[`+A` => S2 => M[(S2, A)]]]].setState2)


  override val getState3: Unit => S1 => S2 => M[(S2, (S1, S3))] =
    implicitly[NestF[S1, λ[`+A` => S2 => M[(S2, A)]]]].nest(implicitly[State2F[S3, λ[`+A` => S2 => M[(S2, A)]]]].getState2)

}

// and so on ...

