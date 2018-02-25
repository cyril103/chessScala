package com.chess.engine.pieces

import com.chess.engine.board._
import com.chess.engine.Alliance
import com.chess.engine.board.Tile.{EmptyTile, OccupiedTile}

trait Piece {
  def getPieceValue : Int


  def position : Int
  def alliance : Alliance
  def calculateLegalMove(board : Board): List[Option[Move]]
  def isFirstMove : Boolean

  def movePiece(move: Move) : Piece

  protected def selectmoves(tile : Tile, board: Board) : Option[Move] = tile  match{
    case OccupiedTile(c,p) =>  if (this.alliance != p.alliance)  Some(AttackMove(board,this,c,p)) else None
    case EmptyTile(c) => Some(MajorMove(board,this,c))
  }

}
