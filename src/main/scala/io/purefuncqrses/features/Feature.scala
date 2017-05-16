package io.purefuncqrses.features

import io.purefuncqrses.State1

import scala.collection.immutable
import scala.language.higherKinds

trait SuccessF[M[+ _]] {

  def success[A](a: A): M[A]

  def flatMap[A, B](ma: M[A])(f_a2mb: A => M[B]): M[B]

  //
  // default implementations
  //

  val done: M[Unit] = success(())

  def map[A, B](ma: M[A])(f_a2b: A => B): M[B] =
    flatMap(ma) { a =>
      success(f_a2b(a))
    }

  def app[A, B](ma: M[A])(m_f_a2b: M[A => B]): M[B] =
    flatMap(ma) { a =>
      flatMap(m_f_a2b) { f_a2b =>
        success(f_a2b(a))
      }
    }

  def traverse[A, B](f_a2mb: A => M[B]): immutable.Seq[A] => M[immutable.Seq[B]] =
    as => as.foldRight(success(immutable.Seq[B]())) { (a, m_bs) =>
      app(f_a2mb(a)) {
        app(m_bs) {
          success {
            bs =>
              b =>
                b +: bs
          }
        }
      }
    }

  def forEach[A](as: immutable.Seq[A]): (A => M[Unit]) => M[Unit] =
    f_a2mu => map(traverse(f_a2mu)(as))(_ => ())

  def sequence[A]: immutable.Seq[M[A]] => M[immutable.Seq[A]] =
    traverse[M[A], A](identity)

}

trait FailureF[M[+ _]] {

  def attempt[A](a: => A): M[A]

  def failure[A](throwable: Throwable): M[A]

  def recover[A](ma: M[A])(pf_t2ma: PartialFunction[Throwable, M[A]]): M[A]

}

trait NestStateF[S, M[+ _]] {

  def nestState[A, B](f_a2mb: A => M[B]): A => State1[S, M, B]

}

trait State1F[S1, M[+ _]] {

  val setState: S1 => M[Unit]
  val getState: Unit => M[S1]

}

trait State2F[S2, M[+ _]] {

  val setState2: S2 => M[Unit]
  val getState2: Unit => M[S2]

}

trait State3F[S3, M[+ _]] {

  val setState3: S3 => M[Unit]
  val getState3: Unit => M[S3]

}

// and so on ...

trait RunF[M[+ _]] {

  type Input

  def run[A, Output](ma: M[A]): Input => Output

}
