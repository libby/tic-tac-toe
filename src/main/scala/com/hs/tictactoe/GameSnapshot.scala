package com.hs.tictactoe

/**
 * a snapshot of a game.
 * goFirst which player should go first, 0 based list, the rest of the
 * players should execute in a sequential order.
 */
case class GameSnapshot(players: List[Player], b: Board, goFirst: Int) {

  private var currentPlayerIndex = goFirst
  private val board = b

  private def setNextPlayer() = {
    currentPlayerIndex = currentPlayerIndex + 1
    currentPlayerIndex = currentPlayerIndex % players.size
  }

  def currentPlayer = {
    players(currentPlayerIndex)
  }

  def displayGame() = {
    board.print()
  }

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
   def placeMark(p: Player, pos: Position): Either[GameError, GameState] = {
      // out of turn
      if (p.mark != currentPlayer.mark) Left(OutOfTurnError)
      else { // try to place the mark
        board.mark(p.mark, pos) match {

          case Right(board) if (Game.checkForWin(board, p)) =>
            println(s"player ${p.mark.symbol} won!! Game Over")
            Right(Won(p))

          case Right(board) if (board.filled) =>
            println(s"Game Over - no winners")
            Right(BoardFilled)

          case Right(board) => // could place mark and the game is not over
            println(s"successfully placed mark")
            setNextPlayer()
            Right(NextPlayer(currentPlayer))

          case Left(error: AlreadyMarked) => // could not place mark
            println(s"Could not place mark")
            Left(SpaceFilledError)

          case Left(error: NotOnBoard) =>
            println(s"Could not place mark")
            Left(PositionNotOnBoard(error.pos))
        }
      }
    }

    def isMarked(pos: Position): Boolean = {
        board.isMarked(pos)
    }

}
