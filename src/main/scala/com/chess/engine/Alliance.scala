package com.chess.engine

import com.chess.engine.player.{BlackPlayer, Player, WhitePlayer}

sealed trait Alliance{
  val direction: Int = this match{
    case WHITE => -1
    case BLACK => 1
  }
  val isWhite : Boolean = this match {
    case WHITE => true
    case BLACK => false
  }
  val isBlack : Boolean =this match {
    case WHITE => false
    case BLACK => true
  }
  def choosePlayer (whitePlayer: WhitePlayer, blackPlayer: BlackPlayer): Player
}

case object WHITE extends Alliance{

  override def choosePlayer(whitePlayer: WhitePlayer, blackPlayer: BlackPlayer): WhitePlayer = whitePlayer
}

case object BLACK extends Alliance{

  override def choosePlayer(whitePlayer: WhitePlayer, blackPlayer: BlackPlayer): BlackPlayer = blackPlayer
}

