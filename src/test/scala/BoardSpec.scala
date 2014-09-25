import com.hs.tictactoe
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
 * > test-only BoardSpec
 */
class BoardSpec extends FlatSpec with Matchers with BeforeAndAfter {
    
    val p1 = PlayerX("X")
    val p2 = PlayerY("Y")
    
    
    "When creating a new board no positions" should " be marked  " in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        board.unmarkedPositions.size should be (9)
    }
    
    "When creating a new board all positions" should " be unmarked  " in {
        val board = tictactoe.Board(3,3)
        board.unmarkedPositions.size should be (9)
    }
    
    "marking a position" should " make the marked position increment by 1 " in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posOneOne = Position(1,1)
        val oneMarkedBoarded = board.mark(board, MarkX, posOneOne)
        board.markedPositions.size should be (1)
    }
    
    "marking a position" should " make unmarked position size decrease by 1" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posOneOne = Position(1,1)
        val oneMarkedBoarded = board.mark(board, MarkX, posOneOne)
        board.unmarkedPositions.size should be (8)
    }
    
    "marking a position with markX " should " set that position as marked " in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posOneOne = Position(1,1)
        val oneMarkedBoarded = board.mark(board, MarkX, posOneOne)
        board.isMarked(posOneOne) should be (true)
    }
    
    "marking a position with markX" should " return that markX when queried" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posOneOne = Position(1,1)
        board.mark(board, MarkX, posOneOne)
        board.getMark(posOneOne) should be (MarkX)
        // can't mark a space twice
        val maybeMarkedAgain = board.mark(board, MarkY, posOneOne)
        maybeMarkedAgain should be (Left(AlreadyMarkedBy(MarkX)))
    }
    
    "marking a position with markY" should " return that markY when queried" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posOneOne = Position(2,1)
        board.mark(board, MarkY, posOneOne)
        board.getMark(posOneOne) should be (MarkY)
    }
    
    "marking a whole row with markY" should " return true when asked if any row is filled by markX" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posZeroZero = Position(0,0)
        val posZeroOne = Position(0,1)
        val posZeroTwo = Position(0,2)
        
        board.mark(board, MarkX, posZeroZero)
        Board.rowFilled(board, MarkX) should be (false)
        board.mark(board, MarkX, posZeroOne)
        Board.rowFilled(board, MarkX) should be (false)
        board.mark(board, MarkX, posZeroTwo)
        board.getMark(posZeroZero) should be (MarkX)
        
        Board.rowFilled(board, MarkX) should be (true)
    }
    
    "marking a whole row with markX except one column " should " return false when asked if any row is filled by markX" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posZeroZero = Position(0,0)
        val posZeroOne = Position(0,1)
        
        board.mark(board, MarkX, posZeroZero)
        board.mark(board, MarkX, posZeroOne)
        
        board.getMark(posZeroZero) should be (MarkX)
        
        Board.rowFilled(board, MarkX) should be (false)
    }
    
    "marking a whole column with markX  " should " return true when asked if any column is filled by markX" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posZeroZero = Position(0,0)
        val posOneZero = Position(1,0)
        val posTwoZero = Position(2,0)
        
        board.mark(board, MarkX, posZeroZero)
        Board.colFilled(board, MarkX) should be (false)
        board.mark(board, MarkX, posOneZero)
        Board.colFilled(board, MarkX) should be (false)
        board.mark(board, MarkX, posTwoZero)
        
        board.getMark(posZeroZero) should be (MarkX)
        
        Board.colFilled(board, MarkX) should be (true)
    }
    
    "marking a whole column with markX except one row " should " return false when asked if any column is filled by markX" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posZeroZero = Position(0,0)
        val posOneZero = Position(1,0)
        
        board.mark(board, MarkX, posZeroZero)
        board.mark(board, MarkX, posOneZero)
        
        board.getMark(posZeroZero) should be (MarkX)
        
        Board.colFilled(board, MarkX) should be (false)
    }
    
    "marking a whole diagonal with markY except one column " should " return false when asked if any diagonal is marked by markY" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        
        val posZeroZero = Position(0,0)
        val posOneOne = Position(1,1)
        val posTwoTwo = Position(2,2)
        
        board.mark(board, MarkX, posZeroZero)
        board.mark(board, MarkX, posOneOne)
        board.mark(board, MarkX, posTwoTwo)
        
        board.getMark(posTwoTwo) should be (MarkX)
        
        Board.diagonalFilled(board, MarkX) should be (true)
    }
    
}
