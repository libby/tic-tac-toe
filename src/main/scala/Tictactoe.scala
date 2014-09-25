import com.hs.tictactoe._

/**
 * A console version of the H.S. tic-tac-toe game.
 */
object Tictactoe extends App {

  var gameSession = new GameSession()

  class GameSession() {
    val p1 = PlayerX("X")
    val p2 = PlayerO("O")
    val game = Game.newGame(p1, p2, 0)
    def currentPlayer = game.currentPlayer
    val board = Board(3, 3)
  }

  def newGame() = {
    gameSession = new GameSession()
    gameSession.game.displayGame()
    playGame(gameSession)
  }

  def welcomeMessage() {
    println()
    println(s"Welcome to tic tac toe ")
    println(s"Enter position of where to place mark ")
    println(s"as a 0 - board length integers followed ")
    println(s"by a space, i.e. 0 2 top right corner of 3 x 3 board.")
  }

  def signalQuit(s: String): Boolean = {
    if (s == "quit" | s == "q") true
    else false
  }

  def quit() = println(s"Bye, bye!")

  case class InvalidInput()

  /**
   * parses the user input into a valid [[com.hs.tictactoe.Position]],
   * note: this does not check that the postion is a a legal position on the
   * current [[com.hs.tictactoe.Board]] for
   * the [[com.hs.tictactoe.GameSnapshot]], but only checks the the input
   * conforms to the console version of the tic-tac-toe game
   *
   * @param input to parse into a possible [[com.hs.tictactoe.Position]]
   * @return
   */
  def maybePositionInput(input: String): Either[InvalidInput, Position] = {
    // support for any size tic-tac-toe board, i.e. 12 3
    val ValidInputRegex = """(\d+)\s(\d+)""".r
    input match {
        case ValidInputRegex(x, y) => Right(Position(x.toInt, y.toInt))
        case _ => Left(InvalidInput())
    }
  }

  def playAgainPrompt() = {
    print(s"Would you like to play again? [y/n] : ")
    val input = System.console().readLine();
    if (input == "y") {
      newGame()
    } else {
      quit()
    }
  }

  def placeMark(gameSession: GameSession, pos: Position) = {
    val player = gameSession.currentPlayer

    gameSession.game.placeMark(player, pos) match {
      case Right(NextPlayer(p)) =>
        gameSession.game.displayGame()
        playGame(gameSession)

      case Right(w: Won) =>
        println(w.msg)
        gameSession.game.displayGame()
        playAgainPrompt()

      case Right(BoardFilled) =>
        println(BoardFilled.msg)
        gameSession.game.displayGame()
        playAgainPrompt()

      case Left(gameError) =>
        println(gameError.msg)
        gameSession.game.displayGame()
        playGame(gameSession)
    }
  }

  def playGame(gameSession: GameSession): Unit = {
    print(s"${gameSession.currentPlayer.mark}'s turn: ")
    val input = System.console().readLine()
    if (signalQuit(input)) quit()
    else {
      maybePositionInput(input) match {
        case Right(pos: Position) =>
          placeMark(gameSession, pos)

        case Left(_) =>
          println(s"Please enter a valid position, i.e. 1 1")
          gameSession.game.displayGame()
          playGame(gameSession)
      }
    }
  }

  // setup initial game
  welcomeMessage()
  newGame()
}