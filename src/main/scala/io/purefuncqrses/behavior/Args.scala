package io.purefuncqrses.behavior

import io.purefuncqrses.behavior.Behavior.History


trait HasHistory[E] {

  def getHistory: History[E]

}

sealed trait Args[E, AS] extends HasHistory[E] {

  def getOptionalAggregateState: Option[AS]

}

case class HistoryArg[E, AS](history: History[E]) extends Args[E, AS] {

  override def getHistory: History[E] = history

  override def getOptionalAggregateState: Option[AS] =
    sys.error("Cannot extract optional aggregate state from history argument")

}

case class HistoryAndOptionalAggregateStateArgs[E, AS](history: History[E], optionalAggregateState: Option[AS]) extends Args[E, AS] {

  override def getHistory: History[E] = history

  override def getOptionalAggregateState: Option[AS] = optionalAggregateState

}
