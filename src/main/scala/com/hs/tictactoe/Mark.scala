package com.hs.tictactoe

/**
 * Each player is assigned a Mark for the duration of a game.
 * During their turn, they place a mark on a tic-tac-toe board.
 * Once placed, a Mark cannot be removed.
 * Traditional tic-tac-toe marks are 'X' and 'O'.
 */
sealed trait Mark {
  val symbol: String
}

case object UnMarked extends Mark {
  override val symbol: String = "-"
}

trait PlayerMark extends Mark

case object MarkX extends PlayerMark {
  override val symbol: String = "X"
}

case object MarkO extends PlayerMark {
  override val symbol: String = "O"
}