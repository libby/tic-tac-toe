import com.hs.tictactoe.{Game, Board}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

/**
 * > test-only PlayGameSpec
 */
class PlayGameSpec extends FlatSpec with Matchers with BeforeAndAfter {
    
    val p1 = PlayerX("X")
    val p2 = PlayerY("Y")
    val board = Board(3,3)
    
    "When creating a new Game board no positions" should " be marked  " in {
        val g = Game.newGame(p1, p2, 1)
        val pos = Position(1,2)
        
        g.isMarked(pos) should be (false)
        //g.placeMark(p1, pos)
        //g.isMarked(pos) should be (true)
        
    }
    
    "A player " should " not be able to make two moves in a row " in {
        val g = Game.newGame(p1, p2, 1)
        val pos1 = Position(1,1)
        val pos2 = Position(1,2)
        
        // g.isMarked(pos) should be (false)
        //         g.placeMark(p1, pos1)
        //         
        //         g.turn should be (p2)
        //         g.placeMark(p1, pos1) should be (Left(OutOfTurnError))
    }
    
    "When placing a mark on a board the position " should " be marked  " in {
        val g = Game.newGame(p1, p2, 1)
    }
    
}
