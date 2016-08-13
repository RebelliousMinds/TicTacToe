package game

case class Board(matrix: Col[Option[PlayerChoice]])

case class Row[A]( left: A, middle: A, right: A)
case class Col[A](top: Row[A], middle: Row[A], bottom: Row[A])

case class GameState(board: Board, player: PlayerChoice)

sealed trait UserInput {
  val row: String
  val column: String
}
case class PositionSelection(row: String, column: String) extends UserInput
