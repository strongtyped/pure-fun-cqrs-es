package io.purefuncqrses.features.implicits

import io.purefuncqrses.Identity
import io.purefuncqrses.features.transform.{NestedStateT, StateT}
import io.purefuncqrses.features._

import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

object FeaturesImplicits {

  class IdentitySuccessF extends SuccessF[Identity] {

    override def success[A](a: A): Identity[A] =
      Identity(a)

    override def flatMap[A, B](ia: Identity[A])(f_a2ib: A => Identity[B]): Identity[B] =
      f_a2ib(ia.value)

  }

  trait IdentityFailureF extends FailureF[Identity] {

    override def attempt[A](a: => A): Identity[A] =
      Identity(a)

    override def failure[A](throwable: Throwable): Identity[A] =
      throw throwable

    override def recover[A](ia: Identity[A])(pf_t2ia: PartialFunction[Throwable, Identity[A]]): Identity[A] =
      try {
        ia
      } catch {
        case throwable: Throwable =>
          pf_t2ia(throwable)
      }

  }

  trait IdentityRunF extends RunF[Identity] {

    override type Input = Unit

    override def run[A, Output](ia: Identity[A]): Input => Output =
      _ => ia.value.asInstanceOf[Output]

  }

  implicit val identityF:
    IdentitySuccessF
      with IdentityFailureF
      with IdentityRunF =
    new IdentitySuccessF
      with IdentityFailureF
      with IdentityRunF

  implicit def identityNestF[S, T] = new NestF[S, λ[`+A` => T => Identity[(T, A)]]] {
    def nest[Z, Y](f_z2iy: Z => T => Identity[(T, Y)]): Z => S => T => Identity[(T, (S, Y))] =
      z =>
        s =>
          t => {
            val (tt, y) = f_z2iy(z)(t).value
            Identity((tt, (s, y)))
          }
  }

  class OptionSuccessF extends SuccessF[Option] {

    override def success[A](a: A): Option[A] =
      Some(a)

    override def flatMap[A, B](oa: Option[A])(f_a2ob: A => Option[B]): Option[B] =
      oa.flatMap(f_a2ob)

  }

  trait OptionFailureF extends FailureF[Option] {

    override def attempt[A](a: => A): Option[A] =
      Option(a)

    override def failure[A](throwable: Throwable): Option[A] =
      throw throwable

    override def recover[A](oa: Option[A])(pf_t2oa: PartialFunction[Throwable, Option[A]]): Option[A] =
      try {
        oa
      } catch {
        case throwable: Throwable =>
          pf_t2oa(throwable)
      }

  }

  implicit val optionF:
    OptionSuccessF
      with OptionFailureF =
    new OptionSuccessF
      with OptionFailureF

  class TrySuccessF extends SuccessF[Try] {

    override def success[A](a: A): Try[A] =
      Success(a)

    override def flatMap[A, B](ta: Try[A])(a2tb: A => Try[B]): Try[B] =
      ta.flatMap(a2tb)

  }

  trait TryFailureF extends FailureF[Try] {

    override def attempt[A](a: => A): Try[A] =
      Try(a)

    override def failure[A](throwable: Throwable): Try[A] =
      Failure(throwable)

    override def recover[A](ta: Try[A])(pf_t2ta: PartialFunction[Throwable, Try[A]]): Try[A] =
      ta.recoverWith(pf_t2ta)

  }

  trait TryRunF extends RunF[Try] {

    override type Input = Unit

    override def run[A, Output](ta: Try[A]): Input => Output = _ => (ta match {
      case Success(a) =>
        a
      case Failure(throwable) =>
        throw throwable
    }).asInstanceOf[Output]

  }

  implicit val tryF:
    TrySuccessF
      with TryFailureF
      with TryRunF =
    new TrySuccessF
      with TryFailureF
      with TryRunF

  import scala.concurrent.ExecutionContext.Implicits.global

  class FutureSuccessF extends SuccessF[Future] {

    override def success[A](a: A): Future[A] =
      Future.successful(a)

    override def flatMap[A, B](fa: Future[A])(a2fb: A => Future[B]): Future[B] =
      fa.flatMap(a2fb)

  }

  trait FutureFailureF extends FailureF[Future] {

    override def attempt[A](a: => A): Future[A] =
      Future.fromTry(Try(a))

    override def failure[A](throwable: Throwable): Future[A] =
      Future.failed(throwable)

    override def recover[A](fa: Future[A])(pf_t2fa: PartialFunction[Throwable, Future[A]]): Future[A] =
      fa.recoverWith(pf_t2fa)

  }

  trait FutureRunF extends RunF[Future] {

    val atMost: Duration = 10000.seconds

    override type Input = Unit

    override def run[A, Output](fa: Future[A]): Input => Output =
      _ => Await.result(fa, atMost).asInstanceOf[Output]

  }

  implicit val futureF:
    FutureSuccessF
      with FutureFailureF
      with FutureRunF =
    new FutureSuccessF
      with FutureFailureF
      with FutureRunF

  implicit def identityStateF[S]:
  SuccessF[λ[`+A` => S => Identity[(S, A)]]]
    with FailureF[λ[`+A` => S => Identity[(S, A)]]]
    with RunF[λ[`+A` => S => Identity[(S, A)]]]
    with StateF[S, λ[`+A` => S => Identity[(S, A)]]] =
    new StateT[S, Identity]

  implicit def tryStateF[S]:
  SuccessF[λ[`+A` => S => Try[(S, A)]]]
    with FailureF[λ[`+A` => S => Try[(S, A)]]]
    with RunF[λ[`+A` => S => Try[(S, A)]]]
    with StateF[S, λ[`+A` => S => Try[(S, A)]]] =
    new StateT[S, Try]

  implicit def futureStateF[S]:
  SuccessF[λ[`+A` => S => Future[(S, A)]]]
    with FailureF[λ[`+A` => S => Future[(S, A)]]]
    with RunF[λ[`+A` => S => Future[(S, A)]]]
    with StateF[S, λ[`+A` => S => Future[(S, A)]]] =
    new StateT[S, Future]

  // TODO: other combinations
  //
  implicit def identityIdentityNestedStateF[S, T]:
  SuccessF[λ[`+A` => S => T => Identity[(T, (S, A))]]]
    with FailureF[λ[`+A` => S => T => Identity[(T, (S, A))]]]
    with RunF[λ[`+A` => S => T => Identity[(T, (S, A))]]]
    with StateF[S, λ[`+A` => S => T => Identity[(T, (S, A))]]]
    with NestedStateF[T, λ[`+A` => S => T => Identity[(T, (S, A))]]] =
    new NestedStateT[S, T, λ[`+A` => T => Identity[(T, A)]]]

}
