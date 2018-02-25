package com.chess.engine.pieces

import com.chess.engine.Alliance
import com.chess.engine.board._

case class Knight(position : Int, alliance : Alliance , isFirstMove : Boolean = true) extends Piece {
    import Knight._

  override def getPieceValue: Int = value

  override def calculateLegalMove(board: Board): List[Option[Move]] = (for{
    candidateMoves: Int <- CANDIDATE_MOVE_COORDINATE

    if BoardUtil.isValidTileCoord(this.position + candidateMoves)
    if !(isExclusion(1)(this.position,candidateMoves) ||
    isExclusion(2)(this.position,candidateMoves) ||
    isExclusion(7)(this.position,candidateMoves) ||
    isExclusion(8)(this.position,candidateMoves))
    tile = board.tile(this.position + candidateMoves)

  }
    yield selectmoves(tile,board)).filter( _.nonEmpty)

  override def toString = "N"

  override def movePiece(move: Move): Knight = Knight(move.coordDest,move.movedPiece.alliance, isFirstMove = false)
}


object Knight{

  private val CANDIDATE_MOVE_COORDINATE = List(6,10,15,17,-6,-10,-15,-17)
  private val value = 300

  private def isExclusion(column: Int)(currentPos :Int, offset :Int) =  {
    val offsetSet = column match{
      case 1 => Set[Int](-17,-10,6,15)
      case 2 => Set[Int](-10,6)
      case 7 => Set[Int](-6,10)
      case 8 => Set[Int](-15,-6,10,17)
    }
  BoardUtil.columsExclusion (column) (currentPos) && offsetSet.contains (offset)
  }

}
