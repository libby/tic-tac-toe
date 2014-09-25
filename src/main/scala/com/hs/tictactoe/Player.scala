package com.hs.tictactoe

/**
 * Represents a tic-tac-toe player, tic-tac-toe players have marks
 * traditionally 'X' and 'O' which they place on a tic-tac-toe
 * board, alternating turns, in an attempt to fill an entire
 * row: horizontally, vertically, or diagonally.
 */
sealed trait Player {
    val mark: Mark
    val name: String
}

case class PlayerX(name: String) extends Player {
    override val mark: PlayerMark = MarkX
}

case class PlayerO(name: String) extends Player {
    override val mark: PlayerMark = MarkO
}