package io.purefuncqrses.util

import io.purefuncqrses.samples.raffle.behavior.AbstractRaffleBehavior.RaffleHistory
import io.purefuncqrses.samples.raffle.behavior.RaffleState
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
    def getRaffleHistory: RaffleHistory = hList.asInstanceOf[shapeless.::[RaffleHistory, HNil]].head

    def getOptionalRaffleState: Option[RaffleState] = hList.asInstanceOf[shapeless.::[RaffleHistory, shapeless.::[Option[RaffleState], HNil]]].tail.head
  }

}
