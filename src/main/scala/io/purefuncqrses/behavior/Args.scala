package io.purefuncqrses.behavior

import io.purefuncqrses.behavior.Behavior.History


trait HasHistory[E] {

  def getHistory: History[E]

}

sealed trait Args[E, AS] extends HasHistory[E] {

  def getOptionalAggregateState: Option[AS]

}

case class HistoryArg[E, AS](raffleHistory: History[E]) extends Args[E, AS] {

  override def getHistory: History[E] = raffleHistory

  override def getOptionalAggregateState: Option[AS] =
    sys.error("Cannot extract optional aggregate state from history argument")

}

case class HistoryAndOptionalAggregateStateArgs[E, AS](raffleHistory: History[E], optionalRaffleState: Option[AS]) extends Args[E, AS] {

  override def getHistory: History[E] = raffleHistory

  override def getOptionalAggregateState: Option[AS] = optionalRaffleState

}
