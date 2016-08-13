package game

sealed trait PlayerChoice
case object Xs extends PlayerChoice
case object Os extends PlayerChoice

