package revised.model

import scala.annotation.tailrec
import scala.io.StdIn._

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

    val topCells = cells.filter( cell => cell.cellPosition.v == Top )
    val middleCells = cells.filter( cell => cell.cellPosition.v == VerticalCenter )
    val bottomCells = cells.filter( cell => cell.cellPosition.v == Bottom )

    println()
    println(" -------------")
    printRow( topCells )
    printRow( middleCells )
    printRow( bottomCells )
    println(" -------------")
    println()

  }
}

object GameState {

  def playerXMoves( gameState: GameState, playerXPosition: PlayerXPosition ) : GameState = {

    val updatedCells = gameState.cells.map( cell => cell.cellPosition match {
      case playerXPosition.cellPosition => Cell(playerXPosition.cellPosition, X)
      case _ => cell
    })

    val gameStatus = computeGameStatus( gameState )
    GameState(updatedCells, PlayerO, gameStatus)

  }

  def playerOMoves( gameState: GameState, playerOPosition: PlayerOPosition ) : GameState = {

    val updatedCells = gameState.cells.map( cell => cell.cellPosition match {
      case playerOPosition.cellPosition => Cell(playerOPosition.cellPosition, O)
      case _ => cell
    })

    val gameStatus = computeGameStatus( gameState )
    GameState(updatedCells, PlayerX, gameStatus)

  }

}

case class Game() {

  def getInput : Option[PositionSelection] = {
    val input = readLine("Where would you like to place your move? [row,column] ")
    input match {
      case "" => None
      case _ => if( input.contains(",") ) {
        val coordinates = input.split(",")
        Some(PositionSelection(coordinates(0), coordinates(1)))
      } else {
        None
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

    val positionSelectionOption = getInput

    positionSelectionOption match {
      case Some(positionSelection) => {
        val cellPosition = toCellPosition(positionSelection)

        cellPosition match {
          case Some(position) => gameState.player match {
            case PlayerX => val newGameState = GameState.playerXMoves(gameState, PlayerXPosition(position))
              newGameState.gameStatus match {
                case InProcess => printGameState(newGameState); loop(newGameState)
                case PlayerXWon => println("Player X Won!")
                case PlayerOWon => println("Player O Won!")
                case Tie => println("Tie, nobody wins!")
              }
            case PlayerO => val newGameState = GameState.playerOMoves(gameState, PlayerOPosition(position))
              newGameState.gameStatus match {
                case InProcess => printGameState(newGameState); loop(newGameState)
                case PlayerXWon => println("Player X Won!")
                case PlayerOWon => println("Player O Won!")
                case Tie => println("Tie, nobody wins!")
              }
          }
          case None => println("Invalid position entered\n"); loop(gameState)
        }

      }
      case None => println("The position you entered wasn't understood.\n"); loop(gameState)
    }


  }

  def play( gameState: GameState) = {
    printGameState(gameState)
    loop(gameState)
  }


  def printGameState(gameState: GameState) : Unit = {
    gameState.print()
  }

}
