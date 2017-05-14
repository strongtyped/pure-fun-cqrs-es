package io.purefuncqrses.features.ops

import io.purefuncqrses.features.{FailureF, SuccessF}

import scala.language.higherKinds

object FeatureOps {

  implicit class SuccessFeatureOps[M[+ _] : SuccessF, A](ma: M[A]) {
    val implicitSuccessF: SuccessF[M] = implicitly[SuccessF[M]]

    def map[B](f_a2b: A => B): M[B] =
      implicitSuccessF.map[A, B](ma)(f_a2b)

    def seq[B](b: => B): M[B] = map { _ =>
      b
    }

    def ignore: M[Unit] = seq(())

    def flatMap[B](f_a2mb: A => M[B]): M[B] =
      implicitSuccessF.flatMap[A, B](ma)(f_a2mb)

    def flatSeq[B](mb: => M[B]): M[B] = flatMap { _ =>
      mb
    }

  }

  implicit class FailureFeatureOps[M[+ _] : FailureF, A](ma: M[A]) {

    val implicitFailureF: FailureF[M] = implicitly[FailureF[M]]

    def recover(pf_t2ma: PartialFunction[Throwable, M[A]]): M[A] =
      implicitFailureF.recover[A](ma)(pf_t2ma)

  }

}