package io.purefuncqrses.features.transform

import io.purefuncqrses.{State1, State2, State3}
import io.purefuncqrses.features._

import scala.language.higherKinds

class State1T[S1, M[+ _] : SuccessF : FailureF : RunF]
  extends SuccessF[λ[`+A` => State1[S1, M, A]]]
    with FailureF[λ[`+A` => State1[S1, M, A]]]
    with RunF[λ[`+A` => State1[S1, M, A]]]
    with State1F[S1, λ[`+A` => State1[S1, M, A]]] {

  val implicitSuccessF: SuccessF[M] = implicitly[SuccessF[M]]
  val implicitFailureF: FailureF[M] = implicitly[FailureF[M]]
  val implicitRunF: RunF[M] = implicitly[RunF[M]]

  override def success[A](a: A): State1[S1, M, A] =
    s =>
      implicitSuccessF.success((s, a))

  override def flatMap[A, B](sma: State1[S1, M, A])(f_a2smb: A => State1[S1, M, B]): State1[S1, M, B] =
    s =>
      implicitSuccessF.flatMap(sma(s)) {
        case (s, a) =>
          f_a2smb(a)(s)
      }

  override def failure[A](throwable: Throwable): State1[S1, M, A] =
    s =>
      implicitFailureF.failure(throwable)

  override def attempt[A](a: => A): State1[S1, M, A] =
    s =>
      implicitFailureF.attempt((s, a))

  override def recover[A](sma: State1[S1, M, A])(pf_t2sma: PartialFunction[Throwable, State1[S1, M, A]]): State1[S1, M, A] =
    s =>
      implicitFailureF.recover(sma(s)) {
        case throwable =>
          pf_t2sma(throwable)(s)
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

  override def run[A, Output](sma: State1[S1, M, A]): Input => Output = {
    case (s, i) =>
      implicitRunF.run(sma(s))(i).asInstanceOf[Output]
  }

}

class State2T[S1, S2, M[+ _] : SuccessF : FailureF : State1F[S2, ?[_]] : NestStateF[S1, ?[_]] : RunF]
  extends State1T[S1, M]
    with State2F[S2, λ[`+A` => State1[S1, M, A]]] {

  override val setState2: State2[S1, S2, M, Unit] =
    implicitly[NestStateF[S1, M]].nestState(implicitly[State1F[S2, M]].setState1)

  override val getState2: State2[S1, Unit, M, S2] =
    implicitly[NestStateF[S1, M]].nestState(implicitly[State1F[S2, M]].getState1)

}

//   type State3[S1, S2, S3, M[+ _], A] = S3 => State2[S2, S1, M, (S1, A)]


class State3T[S1, S2, S3, M[+ _] : SuccessF : FailureF : State1F[S2, ?[_]] : λ[`M[+ _]` => State2F[S3, λ[`+A` => State1[S2, M, A]]]] : NestStateF[S1, ?[_]] : λ[`M[+ _]` => NestStateF[S1, λ[`+A` => State1[S2, M, A]]]] : RunF]
  extends State2T[S1, S2, M]
    with State3F[S3, λ[`+A` => State2[S2, S1, M, (S1, A)]]] {

  override val setState3: State3[S1, S2, S3, M, Unit] =
    implicitly[NestStateF[S1, λ[`+A` => State1[S2, M, A]]]].nestState(implicitly[State2F[S3, λ[`+A` => State1[S2, M, A]]]].setState2)


  override val getState3: State3[S1, S2, Unit, M, S3] =
    implicitly[NestStateF[S1, λ[`+A` => State1[S2, M, A]]]].nestState(implicitly[State2F[S3, λ[`+A` => State1[S2, M, A]]]].getState2)

}

// and so on ...

