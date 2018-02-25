package com.chess.engine.player

import com.chess.engine.board.{Board, Move}

case class MoveTransition (transitionBoard : Board,
                           move : Move,
                           moveStatus : MoveStatus) {

}
