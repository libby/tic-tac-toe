package com.hs.tictactoe

/**
 * Represents an active game.
 *
 * goFirst which player should go first, 0 based list, the rest of the
 * players should execute in a sequential order.
 */
sealed trait Game {

  val players: List[Player]
  val board: Board


  def isMarked(pos: Position): Boolean = {
    board.isMarked(pos)
  }

  def displayString(): String = {
    board.toString
  }
}

sealed trait GameTracker {

  val previousStates: List[Game]
  val allStates: List[Game]

  def replay(): List[Game] = {
    allStates.reverse
  }

}

case class ActiveGame(players: List[Player],
                       board: Board,
                       turnIndex: Int,
                       previousStates: List[Game]
                       ) extends Game with GameTracker {

  val allStates = this :: previousStates

  def currentPlayer = players(turnIndex)

  private def nextPlayerIndex: Int = { (turnIndex + 1 ) % players.size }

  /**
   * Try to place a mark on the board
   *
   * @param p com.hs.tictactoe.Player to move
   * @param pos is a [[com.hs.tictactoe.Position]] on
   *            a [[com.hs.tictactoe.Board]] that the player wishes to put
   *            their mark on.
   * @return Either the next [[com.hs.tictactoe.Player]] to go, or a
   *         GameError if something went wrong.
   */
  def placeMark(p: Player, pos: Position): Either[GameError, Game] = {
    // out of turn
    if (p.mark != currentPlayer.mark) Left(OutOfTurnError)
    else { // try to place the mark
      board.mark(p.mark, pos) match {

        case Right(board) if (Game.checkForWin(board, p)) =>
          Right(WonGame(this.players, board, p, this :: previousStates))

        case Right(board) if (board.filled) =>
          Right(BoardFilledGame(this.players, board, this :: previousStates))

        case Right(board) => // could place mark and the game is not over
          Right(ActiveGame(this.players, board, nextPlayerIndex, this :: previousStates))

        case Left(error: AlreadyMarked) => // could not place mark
          Left(SpaceFilledError)

        case Left(error: NotOnBoard) =>
          Left(PositionNotOnBoard(error.pos))
      }
    }
  }

}

/**
 *
 * @param players
 * @param board
 * @param winner
 * @param previousStates
 */
case class WonGame(players: List[Player],
                   board: Board,
                   winner: Player,
                   previousStates: List[Game]) extends Game with GameTracker {
  val allStates = this :: previousStates
  val msg = s"com.hs.tictactoe.Player ${winner.mark.symbol} won!"

}

/**
 *
 * @param players
 * @param board
 * @param previousStates
 */
case class BoardFilledGame(players: List[Player],
                            board: Board,
                            previousStates: List[Game])
  extends Game with GameTracker {

  val allStates = this :: previousStates
  val msg = "Game over the board has no more free spaces."
}


/**
 * Game helper for creating new Games and checking Game state.
 */
object Game {

  def newGame(player1: Player,
              player2: Player,
              goFirst: Int): ActiveGame = {
      val board: Board = Board()
      ActiveGame(List(player1, player2), board, goFirst, Nil)
  }

  def checkForWin(b: Board, player: Player): Boolean = {
      Board.diagonalFilled(b, player.mark) ||
      Board.colFilled(b, player.mark) ||
      Board.rowFilled(b, player.mark)
  }
}

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