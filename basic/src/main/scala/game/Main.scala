package game

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main extends App {

  println("\n\nShall we play a game?\n\n")

  def togglePlayer(state: GameState): PlayerChoice = {

    if(state.player == Xs )
      Os
    else
      Xs
  }

  def weHaveAWinner(state: GameState): Boolean = {

    state.board.matrix match {

      case Col(Row(Some(Xs), Some(Xs), Some(Xs)), _, _) => true
      case Col(_, Row(Some(Xs), Some(Xs), Some(Xs)), _) => true
      case Col(_, _, Row(Some(Xs), Some(Xs), Some(Xs))) => true

      case Col(Row(Some(Xs), _, _), Row(Some(Xs), _, _), Row(Some(Xs), _, _)) => true
      case Col(Row(_, Some(Xs), _), Row(_, Some(Xs), _), Row(_, Some(Xs), _)) => true
      case Col(Row(_, _, Some(Xs)), _, Row(_, _, Some(Xs))) => true

      case Col(Row(Some(Os), Some(Os), Some(Os)), _, _) => true
      case Col(_, Row(Some(Os), Some(Os), Some(Os)), _) => true
      case Col(_, _, Row(Some(Os), Some(Os), Some(Os))) => true

      case Col(Row(Some(Os), _, _), Row(Some(Os), _, _), Row(Some(Os), _, _)) => true
      case Col(Row(_, Some(Os), _), Row(_, Some(Os), _), Row(_, Some(Os), _)) => true
      case Col(Row(_, _, Some(Os)), Row(_, _, Some(Os)), Row(_, _, Some(Os))) => true

      case Col(Row(Some(Xs), _, _), Row(_, Some(Xs), _), Row(_, _, Some(Xs))) => true
      case Col(Row(_, _, Some(Xs)), Row(_, Some(Xs), _), Row(Some(Xs), _, _)) => true

      case Col(Row(Some(Os), _, _), Row(_, Some(Os), _), Row(_, _, Some(Os))) => true
      case Col(Row(_, _, Some(Os)), Row(_, Some(Os), _), Row(Some(Os), _, _)) => true

      case _ => false

    }

  }

  def update(positionSelection: PositionSelection, state: GameState) : Either[UserState, GameState] = {

    println(s"The user chose row ${positionSelection.row}, column ${positionSelection.column} ...")

    //find the position on the board
    positionSelection match {

      //top row
      case PositionSelection("1", "1") => {

        val oldTop = state.board.matrix.top

        if(oldTop.left.isEmpty) {
          val newTop = oldTop.copy(left = Some(state.player))
          val newState = replaceTopRow(state, newTop)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      case PositionSelection("1", "2") => {

        val oldTop = state.board.matrix.top

        if(oldTop.middle.isEmpty) {
          val newTop = oldTop.copy(middle = Some(state.player))
          val newState = replaceTopRow(state, newTop)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      case PositionSelection("1", "3") => {

        val oldTop = state.board.matrix.top

        if(oldTop.right.isEmpty) {
          val newTop = oldTop.copy(right = Some(state.player))
          val newState = replaceTopRow(state, newTop)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      //middle row
      case PositionSelection("2", "1") => {

        val oldMiddle = state.board.matrix.middle

        if(oldMiddle.left.isEmpty) {
          val newMiddle = oldMiddle.copy(left = Some(state.player))
          val newState = replaceMiddleRow(state, newMiddle)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      case PositionSelection("2", "2") => {

        val oldMiddle = state.board.matrix.middle

        if(oldMiddle.middle.isEmpty) {
          val newMiddle = oldMiddle.copy(middle = Some(state.player))
          val newState = replaceMiddleRow(state, newMiddle)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      case PositionSelection("2", "3") => {

        val oldMiddle = state.board.matrix.middle

        if(oldMiddle.right.isEmpty) {
          val newMiddle = oldMiddle.copy(right = Some(state.player))
          val newState = replaceMiddleRow(state, newMiddle)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      //bottom row
      case PositionSelection("3", "1") => {

        val oldBottom = state.board.matrix.bottom

        if(oldBottom.left.isEmpty) {
          val newBottom = oldBottom.copy(left = Some(state.player))
          val newState = replaceBottomRow(state, newBottom)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      case PositionSelection("3", "2") => {

        val oldBottom = state.board.matrix.bottom

        if(oldBottom.middle.isEmpty) {
          val newBottom = oldBottom.copy(middle = Some(state.player))
          val newState = replaceBottomRow(state, newBottom)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      case PositionSelection("3", "3") => {

        val oldBottom = state.board.matrix.bottom

        if(oldBottom.right.isEmpty) {
          val newBottom = oldBottom.copy(right = Some(state.player))
          val newState = replaceBottomRow(state, newBottom)
          computeNewState(newState)
        } else {
          Left( InvalidMove )
        }

      }

      case PositionSelection(_, _) => Left(InvalidMove)

    }

  }

  def computeNewState(newState: GameState): Either[UserWon, GameState] with Product with Serializable = {
    if (weHaveAWinner(newState)) {
      Left(UserWon(newState))
    } else {
      Right(newState)
    }
  }

  def replaceBottomRow(state: GameState, newBottom: Row[Option[PlayerChoice]]): GameState = {
    val newMatrix = state.board.matrix.copy(bottom = newBottom)
    val newBoard = state.board.copy(matrix = newMatrix)
    state.copy(board = newBoard, player = togglePlayer(state))
  }

  def replaceMiddleRow(state: GameState, newMiddle: Row[Option[PlayerChoice]]): GameState = {
    val newMatrix = state.board.matrix.copy(middle = newMiddle)
    val newBoard = state.board.copy(matrix = newMatrix)
    state.copy(board = newBoard, player = togglePlayer(state))
  }

  def replaceTopRow(state: GameState, newTop: Row[Option[PlayerChoice]]): GameState = {
    val newMatrix = state.board.matrix.copy(top = newTop)
    val newBoard = state.board.copy(matrix = newMatrix)
    state.copy(board = newBoard, player = togglePlayer(state))
  }

  def getInput: PositionSelection = {
    val input = readLine("Where would you like to place your move? [row,column] ")
    val coordinates = input.split(",")
    PositionSelection(coordinates(0), coordinates(1))
  }

  def showError(err: UserState): Unit = {
    println(err)
  }

  def cellToString(cell: Option[PlayerChoice]) = {
    cell match {
      case Some(Xs) => " X "
      case Some(Os) => " O "
      case None => "   "
    }
  }

  def printRow(row: Row[Option[PlayerChoice]]) = {
    row match {
      case Row(l, m, r) => println( s" |${cellToString(l)}|${cellToString(m)}|${cellToString(r)}|" )
    }
  }

  def printLine() = {
    println(" -------------")
  }

  def printBoard(state: GameState): Unit = {

    printLine()
    printRow( state.board.matrix.top )
    printLine()
    printRow( state.board.matrix.middle )
    printLine()
    printRow( state.board.matrix.bottom )
    printLine()

    println( "\n" )

  }

  def printTurn(state: GameState) = {
    println( s"It is ${state.player} turn\n" )
  }

  @tailrec
  def loop(state: GameState): Unit = {

    val userInput = getInput

    update(userInput, state) match {
      case Left(error) => handleUserState(state, error)
      case Right(newState) => printBoard(newState); printTurn(newState); loop(newState)
    }
  }

  def printWinner(state: GameState): Unit = {
    println(s"\n\nWoohoo! Player ${state.player} has won the game.\n\n")
  }

  def handleUserState(oldState: GameState, userState: UserState): Unit = {

    userState match {
      case UserWon(newState) => printBoard(newState); printWinner(oldState)
      case _ =>
        showError(userState)
        printBoard(oldState)
        loop(oldState)
    }

  }

  val initialState = GameState(Board(Col[Option[PlayerChoice]](
    Row(None, None, None),
    Row(None, None, None),
    Row(None, None, None)
  )), Xs)

  printBoard(initialState)

  loop(initialState)

}
