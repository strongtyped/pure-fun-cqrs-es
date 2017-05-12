package io.purefuncqrses.features

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

  def sequence[A]: immutable.Seq[M[A]] => M[immutable.Seq[A]] =
    traverse[M[A], A](identity)

}

trait FailureF[M[+ _]] {

  def attempt[A](a: => A): M[A]

  def failure[A](throwable: Throwable): M[A]

  def recover[A](ma: M[A])(pf_t2ma: PartialFunction[Throwable, M[A]]): M[A]

}

trait StateF[S, M[+ _]] {

  val setState: S => M[Unit]
  val getState: Unit => M[S]

}

trait NestedStateF[T, M[+ _]] {

  val setNestedState: T => M[Unit]
  val getNestedState: Unit => M[T]

}

trait RunF[M[+ _]] {

  type Input

  def run[A, Output](ma: M[A]): Input => Output

}



