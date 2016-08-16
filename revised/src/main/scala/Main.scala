package revised

import revised.model._

object Main extends App {
  println("Let the games begin.")

  println("\n\nShall we play a game?\n\n")

  val leftTop = Cell( CellPosition(Left, Top), Empty )
  val middleTop = Cell( CellPosition(HorizontalCenter, Top), Empty )
  val rightTop = Cell( CellPosition(Right, Top), Empty )

  val leftMiddle = Cell( CellPosition(Left, VerticalCenter), Empty )
  val middleMiddle = Cell( CellPosition(HorizontalCenter, VerticalCenter), Empty )
  val rightMiddle = Cell( CellPosition(Right, VerticalCenter), Empty )

  val leftBottom = Cell( CellPosition(Left, Bottom), Empty )
  val middleBottom = Cell( CellPosition(HorizontalCenter, Bottom), Empty )
  val rightBottom = Cell( CellPosition(Right, Bottom), Empty )

  val cells = List(
    leftTop, middleTop, rightTop,
    leftMiddle, middleMiddle, rightMiddle,
    leftBottom, middleBottom, rightBottom
  )

  val initialGameState = GameState( cells, PlayerX, InProcess )

  val game = Game()

  game.play( initialGameState )

}