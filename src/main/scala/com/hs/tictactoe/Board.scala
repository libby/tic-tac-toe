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
case class Board(rows: Int, columns: Int) {

  private val grid: Array[Array[(Position, Mark)]] = Array.tabulate(3, 3)(
      (x, y) => (Position(x, y), UnMarked)
  )

  /**
   * Print to standard out the current board display state
   */
  def print() = {
    println()
    for {
      rows <- grid
    } yield {
      println(rows.map { x => x._2.symbol }.mkString(" | "))
      if (rows(0)._1.x < this.rows - 1) println("---------")
    }
    println()
  }

  //val gridCt = Array.tabulate(3,3)( (x, y) => (x, y) )
  // for { row <- gridCt; entry <- row } yield { entry._1 }
  // for { row <- grid; entry <- row; if entry == NoMark()} yield { entry } filter {x => x == NoMark()}
  // for { row <- grid; (pos, mark) <- row; if mark == NoMark()} yield { pos }
  def unmarkedPositions: List[Position] = {
      (for {
          row <- grid
          (pos, mark) <- row
          if mark == UnMarked
      } yield { pos }).toList
  }

  def markedPositions: List[Position] = {
      (for {
          row <- grid
          (pos, mark) <- row
          if mark != UnMarked
      } yield pos).toList
  }

  // no more free spaces
  def filled: Boolean = {
     markedPositions.size == rows * columns
  }

  def markedSpacesForPlayer(player: Player): List[Position] = {
      (for {
          row <- grid
          (pos, mark) <- row
          if mark == player.mark
      } yield pos).toList
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
      grid(pos.x)(pos.y) = (pos, mark)
      Right(this)
    }
  }

  def isOnBoard(pos: Position): Boolean = {
    (pos.x > 0 && pos.x < this.rows) && (pos.y > 0 && pos.y < this.columns)
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

  def colFilled(b: Board, mark: Mark): Boolean = {
    val markedColumns = (0 until b.columns).map { c =>
                              (0 until b.rows).forall { r =>
                                  val p = Position(r,c)
                                  b.isMarkedBy(p, mark)
                              }
                          }
    markedColumns.contains(true)
  }

  def rowFilled(b: Board, mark: Mark): Boolean = {

      val markedRows = (0 until b.rows).map { r =>
                            (0 until b.columns).forall { c =>
                                val p = Position(r,c)
                                b.isMarkedBy(p, mark)
                            }
                        }
      markedRows.contains(true)
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