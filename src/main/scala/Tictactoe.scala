import com.hs.tictactoe._

/**
 * A console version of the H.S. tic-tac-toe game.
 */
object Tictactoe extends App {

  object Prompts {

    val PlayAgainPrompt = s"Would you like to play again? [y/n] : "

    val WelcomePrompt =
      """Welcome to tic tac toe
        |Enter position of where to place mark
        |as a 0 - board length integers followed
        |by a space, i.e. 0 2 top right corner of 3 x 3 board.
      """.stripMargin

    val ExitMessage = "Bye, bye!"

  }

  def newGame() = {
    val goFirst = 0
    val newGame = Game.newGame(PlayerX("X"), PlayerO("O"), goFirst)
    println(newGame.displayString())
    playGame(newGame)
  }

  def welcomeMessage() {
    println()
    println(Prompts.WelcomePrompt)
  }

  def signalQuit(s: String): Boolean = {
    (s.equalsIgnoreCase("quit") || s.equalsIgnoreCase("q"))
  }

  def signalReplay(s: String): Boolean = {
    (s.equalsIgnoreCase("replay") || s.equalsIgnoreCase("r"))
  }

  def quit() = println(Prompts.ExitMessage)

  /**
   * parses the user input into a valid [[com.hs.tictactoe.Position]],
   * note: this does not check that the postion is a a legal position on the
   * current [[com.hs.tictactoe.Board]] for
   * the [[com.hs.tictactoe.Game]], but only checks the the input
   * conforms to the console version of the tic-tac-toe game
   *
   * @param input to parse into a possible [[com.hs.tictactoe.Position]]
   * @return
   */
  def maybePositionInput(input: String): Option[Position] = {
    // support for any size tic-tac-toe board, i.e. 12 3
    val ValidPositionRegex = """(\d+)\s+(\d+)""".r
    input match {
        case ValidPositionRegex(x, y) => Some(Position(x.toInt, y.toInt))
        case _ => None
    }
  }

  def playAgainPrompt(gameTracker: GameTracker): Unit = {
    print(Prompts.PlayAgainPrompt)
    val input = System.console().readLine()
    if (input == "y" || input == "yes") newGame()
    else if (input == "n" || input == "no" || signalQuit(input)) quit()
    else if (signalReplay(input)) {
      replayGame(gameTracker)
      playAgainPrompt(gameTracker)
    }
    else {
      println("Sorry I did not understand you \n")
      playAgainPrompt(gameTracker)
    }
  }

  def placeMark(game: ActiveGame, pos: Position) = {
    // Do we need this? game knows...
    val player = game.currentPlayer

    game.placeMark(player, pos) match {

      case Right(activeGame: ActiveGame) =>
        println(activeGame.displayString())
        playGame(activeGame)

      case Right(w: WonGame) =>
        println(w.msg)
        println(w.displayString())
        playAgainPrompt(w)

      case Right(bf: BoardFilledGame) =>
        println(bf.msg)
        println(bf.displayString())
        playAgainPrompt(bf)

      case Left(gameError) => // something went wrong in the move. play same game.
        println(gameError.msg)
        println(game.displayString())
        playGame(game)
    }
  }

  def playGame(game: ActiveGame): Unit = {
    print(s"${game.currentPlayer.mark}'s turn: ")
    val input = System.console().readLine().toLowerCase().trim()
    if (signalQuit(input)) quit()
    else if (signalReplay(input)) {
      replayGame(game)
      playGame(game)
    } else {
      maybePositionInput(input) match {
        case Some(pos: Position) =>
          placeMark(game, pos)

        case None =>
          println(s"Please enter a valid position, i.e. 1 1")
          println(game.displayString())
          playGame(game)
      }
    }
  }

  def replayGame(gameTracker: GameTracker) = {

    def displayGame(game: Game): Unit = {
      println()
      println(game.displayString())
    }
    println(s"Replaying games ${gameTracker.allStates.size} moves so far: \n")
    gameTracker.replay.foreach ( displayGame )
  }

  // setup initial game
  welcomeMessage()
  newGame()
}