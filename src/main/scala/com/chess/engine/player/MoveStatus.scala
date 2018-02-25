package com.chess.engine.player

import com.chess.engine.player.MoveStatus.{DONE, ILLEGAL_MOVE, LEAVES_PLAYER_IN_CHECK}

sealed trait MoveStatus {

    def isDone: Boolean = this match {
      case DONE => true
      case ILLEGAL_MOVE => false
      case LEAVES_PLAYER_IN_CHECK => false
    }

  }

object MoveStatus {

  case object DONE extends MoveStatus

  case object ILLEGAL_MOVE extends MoveStatus

  case object LEAVES_PLAYER_IN_CHECK extends MoveStatus
}

