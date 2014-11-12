package com.hs.tictactoe

import com.hs.tictactoe
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
 * > test-only BoardSpec
 */
class BoardSpec extends FlatSpec with Matchers with BeforeAndAfter {
    
    val p1 = PlayerX("X")
    val p2 = PlayerO("O")
    
    
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
      val oneMarkedBoarded = board.mark(MarkX, posOneOne)
      oneMarkedBoarded.isRight should be (true)
      oneMarkedBoarded.right.get.markedPositions.size should be (1)
    }
    
    "marking a position" should " make unmarked position size decrease by 1" in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
      val posOneOne = Position(1,1)
      val eitherOneMarkedBoarded = board.mark(MarkX, posOneOne)
      eitherOneMarkedBoarded.isRight should be (true)
      eitherOneMarkedBoarded.right.get.unmarkedPositions.size should be (8)
    }

     // TODO: good spot for property based testing.
    "marking a position with markX " should " set that position as marked " in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
      val posOneOne = Position(1,1)
      val maybeMarkedBoard = board.mark(MarkX, posOneOne)
      maybeMarkedBoard.isRight should be (true)
      maybeMarkedBoard.right.get.isMarked(posOneOne) should be (true)
    }
    
    "marking a position with markX" should " return that markX when queried" in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
      val posOneOne = Position(1,1)
      val maybeBoard = board.mark(MarkX, posOneOne)
      maybeBoard.isRight should be (true)
      val markedBoard = maybeBoard.right.get
      markedBoard.getMark(posOneOne) should be (MarkX)
      // can't mark a space twice
      val maybeMarkedAgain = markedBoard.mark(MarkO, posOneOne)
      maybeMarkedAgain should be (Left(AlreadyMarked(Position(1, 1))))
    }
    
    "marking a position with markY" should " return that markY when queried" in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
      val posOneOne = Position(2,1)
      val newBoard = board.mark(MarkO, posOneOne)
      newBoard.isRight should be (true)
      newBoard.right.get.getMark(posOneOne) should be (MarkO)
    }
    
    "marking a whole row with markX" should " return true when asked if any row is filled by markX" in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
      val posZeroZero = Position(0,0)
      val posZeroOne = Position(0,1)
      val posZeroTwo = Position(0,2)

      for {
        m1 <- board.mark(MarkX, posZeroZero).right
        m2 <- m1.mark(MarkX, posZeroOne).right
        m3 <- m2.mark(MarkX, posZeroTwo).right
      } yield {
        Board.rowFilled(m1, MarkX) should be (false)
        Board.rowFilled(m2, MarkX) should be (false)
        Board.rowFilled(m3, MarkX) should be (true)
      }

    }
    
    "marking a whole row with markX except one column " should " return false when asked if any row is filled by markX" in {
        val board = tictactoe.Board(3,3)
        board.markedPositions should be (empty)
        val posZeroZero = Position(0,0)
        val posZeroOne = Position(0,1)

      for {
        m1 <- board.mark(MarkX, posZeroZero).right
        m2 <- m1.mark(MarkX, posZeroOne).right
      } yield {
        m2.getMark(posZeroZero) should be (MarkX)
        Board.rowFilled(m2, MarkX) should be (false)
      }
    }
    
    "marking a whole column with markX  " should " return true when asked if any column is filled by markX" in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
      val posZeroZero = Position(0,0)
      val posOneZero = Position(1,0)
      val posTwoZero = Position(2,0)


      for {
        m1 <- board.mark(MarkX, posZeroZero).right
        m2 <- m1.mark(MarkX, posOneZero).right
        m3 <- m2.mark(MarkX, posTwoZero).right
      } yield {
        Board.colFilled(m1, MarkX) should be (false)
        Board.colFilled(m2, MarkX) should be (false)
        Board.colFilled(m3, MarkX) should be (true)
      }
    }
    
    "marking a whole column with markX except one row " should " return false when asked if any column is filled by markX" in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
      val posZeroZero = Position(0,0)
      val posOneZero = Position(1,0)

      for {
        m1 <- board.mark(MarkX, posZeroZero).right
        m2 <- m1.mark(MarkX, posOneZero).right
      } yield {
        Board.colFilled(m1, MarkX) should be (false)
        Board.colFilled(m2, MarkX) should be (false)
      }

    }
    
    "marking a whole diagonal with markY except one column " should " return false when asked if any diagonal is marked by markY" in {
      val board = tictactoe.Board(3,3)
      board.markedPositions should be (empty)
        
      val posZeroZero = Position(0,0)
      val posOneOne = Position(1,1)
      val posTwoTwo = Position(2,2)

      for {
        m1 <- board.mark(MarkX, posZeroZero).right
        m2 <- m1.mark(MarkX, posOneOne).right
        m3 <- m2.mark(MarkX, posTwoTwo).right
      } yield {
        Board.diagonalFilled(m1, MarkX) should be (false)
        Board.diagonalFilled(m2, MarkX) should be (false)
        Board.diagonalFilled(m3, MarkX) should be (true)
      }

    }
    
}
