package game

sealed trait UserState
case object InvalidMove extends UserState
case object GameOver extends UserState
case class UserWon( player: PlayerChoice ) extends UserState

