package com.hs.tictactoe

/**
 * Game helper for creating new Games and checking Game state.
 */
object Game {

    def newGame(player1: Player,
                player2: Player,
                goFirst: Int): GameSnapshot = {
        val board: Board = Board(3, 3)
        GameSnapshot(List(player1, player2), board, goFirst)
    }

    def checkForWin(b: Board, player: Player): Boolean = {
        Board.diagonalFilled(b, player.mark) ||
        Board.colFilled(b, player.mark) ||
        Board.rowFilled(b, player.mark)
    }
}

/**
 * Represents various states a Game can be in.
 */
sealed trait GameState {
  val msg = ""
}

case class NextPlayer(player: Player) extends GameState

case class Won(player: Player) extends GameState {
  override val msg = s"com.hs.tictactoe.Player ${player.mark.symbol} won!"
}

case object BoardFilled extends GameState {
  override val msg = "Game over the board has no more free spaces."
}

//case class GameOver() extends GameState

/**
 * Errors that might occur when a player tries to place a mark.
 */
sealed trait GameError {
  val msg = "Game Error"
}

case object OutOfTurnError extends GameError {
  override val msg = "Sorry, you are playing out of turn!"
}

case object SpaceFilledError extends GameError {
  override val msg = "The space is already filled."
}

case class PositionNotOnBoard(pos: Position) extends GameError {
  override val msg = s"The position ${pos} is not on the board."
}