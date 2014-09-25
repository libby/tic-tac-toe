import com.hs.tictactoe.Board
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
 * > test-only PrintBoardSpec
 */
class PrintBoardSpec extends FlatSpec with Matchers with BeforeAndAfter {

  val p1 = PlayerX("X")
  val p2 = PlayerY("Y")


  "When creating a new board no positions" should " be marked  " in {
    val board = Board(3, 3)
    board.markedPositions should be (empty)
    board.unmarkedPositions.size should be (9)
    board.print()
    println()
    println()
  }

  "marking a position" should " make the marked position increment by 1 " in {
    val board = Board(3,3)
    board.markedPositions should be (empty)
    val posOneOne = Position(1,1)
    val oneMarkedBoarded = board.mark(board, MarkX, posOneOne)
    board.markedPositions.size should be (1)
    board.print()
  }

}
