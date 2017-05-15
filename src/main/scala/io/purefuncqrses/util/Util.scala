package io.purefuncqrses.util

import shapeless.{HList, HNil}

object Util {

  def removeFirst[A](as: List[A])(pred: A => Boolean): List[A] = {
    val index = as.indexWhere(pred)
    if (index >= 0) {
      val (las, ras) = as.splitAt(index)
      las ::: ras.tail
    } else {
      as
    }
  }

  implicit class ListOps[A](as: List[A]) {

    def add(a: A) = a :: as

    def remove(a: A): List[A] = removeFirst(as)(_ == a)

  }

  implicit class UnsafeHListOps(hList: HList) {
    def _1[A]: A = hList.asInstanceOf[shapeless.::[A, HNil]].head
    def _2[A, B]: B = hList.asInstanceOf[shapeless.::[A, shapeless.::[B, HNil]]].tail.asInstanceOf[shapeless.::[B, HNil]].head
  }

}
