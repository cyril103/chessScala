package com.chess.engine.pieces

import com.chess.engine.Alliance
import com.chess.engine.board.{Board, BoardUtil, Move}


case class King (position : Int, alliance : Alliance, isFirstMove : Boolean = true ) extends Piece {
  import King._

  override def getPieceValue: Int = value

  def calculateLegalMove(board: Board): List[Option[Move]] = (for{

    candidateMoves: Int <- CandidateMoveOffset

    if BoardUtil.isValidTileCoord(this.position + candidateMoves)
    if !(isExclusion(1)(this.position,candidateMoves) ||
       isExclusion(8)(this.position,candidateMoves))
    tile = board.tile(this.position + candidateMoves)

  }
    yield selectmoves(tile,board)).filter(_.nonEmpty)


  override def toString = "K"

  override def movePiece(move: Move): King = King(move.coordDest,move.movedPiece.alliance,isFirstMove = false)
}

object King {
  private val CandidateMoveOffset = List(-1,1,8,-8,-9,9,-7,7)
  private val value = 1000

  private def isExclusion(column: Int)(currentPos :Int, offset :Int) =  {
    val offsetSet = column match{
      case 1 => Set[Int](-1,-9,7)
      case 8 => Set[Int](1,-7,9)
    }
    BoardUtil.columsExclusion (column) (currentPos) && offsetSet.contains (offset)
  }
}
