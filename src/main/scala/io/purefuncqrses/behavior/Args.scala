package io.purefuncqrses.behavior

import io.purefuncqrses.behavior.Behavior.History


trait HasHistory[E] {

  def getHistory: History[E]

}

sealed trait Args[E] extends HasHistory[E] {

}

case class HistoryArg[E](history: History[E]) extends Args[E] {

  override def getHistory: History[E] = history

}

case class HistoryAndOptionalAggregateArgs[E, A](history: History[E], optionalAggregate: Option[A]) extends Args[E] {

  override def getHistory: History[E] = history

  def getOptionalAggregate: Option[A] = optionalAggregate

}
