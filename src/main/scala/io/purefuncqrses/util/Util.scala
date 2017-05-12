package io.purefuncqrses.util

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

}
