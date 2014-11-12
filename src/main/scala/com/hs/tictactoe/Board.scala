package com.hs.tictactoe

sealed trait BoardError {
  val msg: String
}

case class NotOnBoard(pos: Position) extends BoardError {
  override val msg: String = s"The position ${pos} is not on the board."
}

case class AlreadyMarked(pos: Position) extends BoardError {
  override val msg: String = s"The position ${pos} is not open."
}

/**
 * Represents a tic-tac-toe board.
 */
case class Board(rows: Int = 3,
                 columns: Int = 3) {

  private val grid: Array[Array[(Position, Mark)]] = Array.tabulate(rows, columns)(
      (x, y) => (Position(x, y), UnMarked)
  )

  def this(g: Array[Array[(Position, Mark)]]) = {
    this(g.size, g(0).size)

    for {
      row <- grid
      (pos, mark) <- row
     } yield grid(pos.x)(pos.y) = g(pos.x)(pos.y).copy()

  }

  def copy2DArray(g: Array[Array[(Position, Mark)]]): Array[Array[(Position, Mark)]] = {
    val newGrid: Array[Array[(Position, Mark)]] = Array.tabulate(g.size, g(0).size)(
      (x, y) => (Position(x, y), UnMarked)
    )
    for {
      row <- g
      (pos, mark) <- row
    } yield newGrid(pos.x)(pos.y) = g(pos.x)(pos.y)
    newGrid
  }

  /**
   * Print to standard out the current board display state
   */
  override def toString(): String  = {
    var s = ""
    for {
      rows <- grid
    } yield {
      s = s + rows.map { x => x._2.symbol }.mkString(" | ") + "\n"
      if (rows(0)._1.x < this.rows - 1) s = s + "---------\n"
    }
    s
  }

  def unmarkedPositions: Seq[Position] = {
      for {
          row <- grid
          (pos, mark) <- row
          if mark == UnMarked
      } yield pos
  }

  def markedPositions: Seq[Position] = {
      for {
          row <- grid
          (pos, mark) <- row
          if mark != UnMarked
      } yield pos
  }

  // no more free spaces
  def filled: Boolean = {
     markedPositions.size == rows * columns
  }

  def markedSpacesForPlayer(player: Player): Seq[Position] = {
      for {
          row <- grid
          (pos, mark) <- row
          if mark == player.mark
      } yield pos
  }

  def isMarked(pos: Position): Boolean = grid(pos.x)(pos.y)._2 != UnMarked

  def isMarkedBy(pos: Position,
                 mark: Mark): Boolean = grid(pos.x)(pos.y)._2 == mark

  def getMark(pos: Position): Mark = grid(pos.x)(pos.y)._2

  /**
   * @param mark to place on board
   * @param pos [[com.hs.tictactoe.Position]] to place mark
   * @return Either the updated board, and an Left Board if
   *         couldn't place the mark
   */
  def mark(mark: Mark, pos: Position): Either[BoardError, Board]  = {
    if (!isOnBoard(pos)) Left(NotOnBoard(pos))
    else if (this.isMarked(pos)) Left(AlreadyMarked(pos))
    else { // valid move, place it!
      //grid(pos.x)(pos.y) = (pos, mark)
      val gridCopy = copy2DArray(grid)
      gridCopy(pos.x)(pos.y) = (pos, mark)
      val newBoard = new Board(gridCopy)
      Right(newBoard)
    }
  }

  def isOnBoard(pos: Position): Boolean = {
    (pos.x >= 0 && pos.x < this.rows) && (pos.y >= 0 && pos.y < this.columns)
  }
}

object Board {

  def diagonalFilled(b: Board, mark: Mark): Boolean = {
    /** uses implicits in Position to map from
      * Seq[Tuple2[Int,Int]] => Seq[Position]
      * see [[com.hs.tictactoe.Position.seqTuple2Position()]]
      */
    val topLeft2BotRight = (0 until b.rows) zip (0 until b.columns)
    val botLeft2TopRight = (0 until b.rows).reverse.zip(0 until b.columns)

    def checkDiagonal(positions: Seq[Position]): Boolean = {
      positions.forall { p =>
         b.isMarkedBy(p, mark)
      }
    }
    checkDiagonal(topLeft2BotRight) || checkDiagonal(botLeft2TopRight)
  }

  type NumOfLines = Int
  type LineLength = Int
  // the line number that is being checked, i.e. could be a row number or col number.
  type LineNumToCheck = Int
  // the index in the line (i.e., row or col) that is being checked.
  type IndexInLineNum = Int

  private [this] def checkStraightLines(b: Board,
                                      mark: Mark,
                                      numOfLines: NumOfLines,
                                      lineLn: LineLength,
                                      posFn: (LineNumToCheck, IndexInLineNum) => Position): Boolean = {
    val markedLines = (0 until numOfLines).map { currentLine =>
      (0 until lineLn).forall { indexInLine =>
        val p = posFn(currentLine, indexInLine)
        b.isMarkedBy(p, mark)
      }
    }
    markedLines.contains(true)
  }

  def colFilled(b: Board, mark: Mark): Boolean = {
    val toColPosition: (LineNumToCheck, IndexInLineNum) => Position = {
      (line, index) => Position(line, index) }
    checkStraightLines(b, mark, b.columns, b.rows, toColPosition)
  }

  def rowFilled(b: Board, mark: Mark): Boolean = {
    val toRowPosition: (LineNumToCheck, IndexInLineNum) => Position = {
      (line, index) => Position(line, index)
    }
    checkStraightLines(b, mark, b.rows, b.columns, toRowPosition)
  }

}

/**
 * Represents a position on a board
 */
case class Position(x: Int, y: Int) {
  override def toString(): String = "[%d, %d]".format(x, y)
}

object Position {

  implicit def tuple2Position(t: (Int, Int)): Position = Position(t)
  implicit def seqTuple2Position(tups: Seq[(Int, Int)]): Seq[Position] = {
    tups.map { t => Position(t) }
  }

  def apply(t: (Int, Int)): Position = {
    Position(t._1, t._2)
  }

}