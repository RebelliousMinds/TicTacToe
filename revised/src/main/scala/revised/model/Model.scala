package revised.model

import scala.annotation.tailrec
import scala.io.StdIn._

sealed trait UserInput
case object Blank extends UserInput
case class PossiblePosition(positionSelection: PositionSelection) extends UserInput
case object InvalidPosition extends UserInput
case object Exit extends UserInput

sealed trait HorizontalPosition
case object Left extends HorizontalPosition
case object HorizontalCenter extends HorizontalPosition
case object Right extends HorizontalPosition

sealed trait VerticalPosition
case object Top extends VerticalPosition
case object VerticalCenter extends VerticalPosition
case object Bottom extends VerticalPosition

case class PositionSelection(row: String, column: String)

case class CellPosition(h: HorizontalPosition, v: VerticalPosition)

sealed trait PlayerPosition
case class PlayerXPosition(cellPosition: CellPosition) extends PlayerPosition
case class PlayerOPosition(cellPosition: CellPosition) extends PlayerPosition

sealed trait Player
case object PlayerX extends Player
case object PlayerO extends Player

sealed trait CellState
case object X extends CellState
case object O extends CellState
case object Empty extends CellState

sealed trait GameStatus
case object InProcess extends GameStatus
case object PlayerXWon extends GameStatus
case object PlayerOWon extends GameStatus
case object Tie extends GameStatus
case object SpotAlreadyTaken extends GameStatus
case object EmptyStatus extends GameStatus

case class Cell( cellPosition: CellPosition, cellState: CellState)

case class GameState(cells: List[Cell], player: Player, gameStatus: GameStatus) {

  def printRow(cells: List[Cell]) : Unit = {

    val cellStr = cells.map( cell => cell match {
      case Cell(CellPosition(_,_),X) => "X"
      case Cell(CellPosition(_,_),O) => "O"
      case Cell(CellPosition(_,_),Empty) => "-"
    }).mkString(
      " | "
    )

    println( s" | $cellStr |")

  }

  def print(): Unit = {

    println()
    println(" -------------")
    printRow( cells.filter( cell => cell.cellPosition.v == Top ) )
    printRow( cells.filter( cell => cell.cellPosition.v == VerticalCenter ) )
    printRow( cells.filter( cell => cell.cellPosition.v == Bottom ) )
    println(" -------------")
    println()

  }
}

object GameState {

  def computeGameStatus(cells: List[Cell] ) : GameStatus = {

    //TODO it would be nice to simplify here somehow

    val topCells = cells.filter(cell => cell.cellPosition.v == Top).map(_.cellState).distinct
    val middleCells = cells.filter(cell => cell.cellPosition.v == VerticalCenter).map(_.cellState).distinct
    val bottomCells = cells.filter(cell => cell.cellPosition.v == Bottom).map(_.cellState).distinct

    val leftCells = cells.filter(cell => cell.cellPosition.h == Left).map(_.cellState).distinct
    val centerCells = cells.filter(cell => cell.cellPosition.h == HorizontalCenter).map(_.cellState).distinct
    val rightCells = cells.filter(cell => cell.cellPosition.h == Right).map(_.cellState).distinct

    val descDiagCells = List(
      cells.find {cell => cell.cellPosition == CellPosition(Left, Top) },
      cells.find {cell => cell.cellPosition == CellPosition(HorizontalCenter, VerticalCenter) },
      cells.find {cell => cell.cellPosition == CellPosition(Right, Bottom) }
    ).map(_.get).map(_.cellState).distinct

    val ascDiagCells = List(
      cells.find {cell => cell.cellPosition == CellPosition(Right, Top) },
      cells.find {cell => cell.cellPosition == CellPosition(HorizontalCenter, VerticalCenter) },
      cells.find {cell => cell.cellPosition == CellPosition(Left, Bottom) }
    ).map(_.get).map(_.cellState).distinct

    val cellStatuses = List(
      topCells,
      middleCells,
      bottomCells,
      leftCells,
      centerCells,
      rightCells,
      descDiagCells,
      ascDiagCells
    )

    cellStatuses.find { cellStatusList => cellStatusList.size == 1 &&
      ( cellStatusList.head == X || cellStatusList.head == O ) } match {

      //There was a row winner
      case Some(status) => status.head match {
        case X => PlayerXWon
        case O => PlayerOWon
        case Empty => EmptyStatus
      }

      //There was no row winner
      case None => cells.find { cell => cell.cellState == Empty } match {
        case Some(_) => InProcess
        case None => Tie
      }

    }

  }

  //TODO get rid of duplication here

  def playerXMoves(gameState: GameState, playerXPosition: PlayerXPosition ) : GameState = {

    gameState.cells.find { cell => cell.cellPosition == playerXPosition.cellPosition && cell.cellState == Empty } match {

      case Some(_) =>
        val updatedCells = gameState.cells.map {
          case Cell(playerXPosition.cellPosition, Empty) => Cell(playerXPosition.cellPosition, X)
          case cell@Cell(playerXPosition.cellPosition, _) => cell
          case cell => cell
        }

        val gameStatus = computeGameStatus( updatedCells )
        GameState(updatedCells, PlayerO, gameStatus)

      case None => gameState.copy( gameStatus = SpotAlreadyTaken )

    }

  }

  def playerOMoves(gameState: GameState, playerOPosition: PlayerOPosition ) : GameState = {

    gameState.cells.find { cell => cell.cellPosition == playerOPosition.cellPosition && cell.cellState == Empty } match {

      case Some(_) =>
        val updatedCells = gameState.cells.map {
          case Cell(playerOPosition.cellPosition, Empty) => Cell(playerOPosition.cellPosition, O)
          case cell@Cell(playerOPosition.cellPosition, _) => cell
          case cell => cell
        }

        val gameStatus = computeGameStatus( updatedCells )
        GameState(updatedCells, PlayerX, gameStatus)

      case None => gameState.copy( gameStatus = SpotAlreadyTaken )

    }

  }

}

case class Game() {

  def getInput : UserInput = {
    val input = readLine("Where would you like to place your move? [row,column] ")
    input match {
      case null => Exit
      case "exit" => Exit
      case "" => Blank
      case _ => if( input.contains(",") ) {
        val coordinates = input.split(",")
        PossiblePosition(PositionSelection(coordinates(0), coordinates(1)))
      } else {
        InvalidPosition
      }
    }

  }

  def toCellPosition(positionSelection: PositionSelection) : Option[CellPosition] = {
    positionSelection match {
      case PositionSelection("1", "1") => Some(CellPosition(Left, Top))
      case PositionSelection("2", "1") => Some(CellPosition(Left, VerticalCenter))
      case PositionSelection("3", "1") => Some(CellPosition(Left, Bottom))
      case PositionSelection("1", "2") => Some(CellPosition(HorizontalCenter, Top))
      case PositionSelection("2", "2") => Some(CellPosition(HorizontalCenter, VerticalCenter))
      case PositionSelection("3", "2") => Some(CellPosition(HorizontalCenter, Bottom))
      case PositionSelection("1", "3") => Some(CellPosition(Right, Top))
      case PositionSelection("2", "3") => Some(CellPosition(Right, VerticalCenter))
      case PositionSelection("3", "3") => Some(CellPosition(Right, Bottom))
      case PositionSelection(_, _) => None
    }
  }

  @tailrec
  final def loop( gameState: GameState ) : Unit = {

    printGameState(gameState)

    gameState.player match {
      case PlayerX => println("It's player X's turn.")
      case PlayerO => println("It's player O's turn.")
    }

    val positionSelectionOption = getInput

    positionSelectionOption match {
      case PossiblePosition(positionSelection) =>
        toCellPosition(positionSelection) match {
          case Some(position) => gameState.player match {
            case PlayerX => matchStatus(GameState.playerXMoves(gameState, PlayerXPosition(position)))
            case PlayerO => matchStatus(GameState.playerOMoves(gameState, PlayerOPosition(position)))
          }
          case None => println("Invalid position entered\n"); loop(gameState)
        }

      case Blank => println("The position you entered wasn't understood.\n"); loop(gameState)
      case InvalidPosition => println("Invalid position entered\n"); loop(gameState)
      case Exit => println("Thank you for playing, goodbye!")
    }

  }

  def matchStatus(gameState: GameState): Unit = {

    gameState.gameStatus match {
      case InProcess => loop(gameState)
      case PlayerXWon => printGameState(gameState); println("Player X Won!")
      case PlayerOWon =>  printGameState(gameState); println("Player O Won!")
      case Tie => printGameState(gameState); println("Tie, nobody wins!")
      case SpotAlreadyTaken => println("Hey that spot's already taken!  Try again..."); loop(gameState)
      case EmptyStatus => println("Empty won the match somehow...")
    }

  }

  def play(gameState: GameState) = {
    loop(gameState)
  }


  def printGameState(gameState: GameState) : Unit = {
    gameState.print()
  }

}
