package com.chess.engine.pieces

import com.chess.engine.Alliance
import com.chess.engine.board.Tile.{EmptyTile, OccupiedTile}
import com.chess.engine.board._

import scala.collection.mutable.ListBuffer

case class Pawn(position : Int, alliance : Alliance ,isFirstMove : Boolean = true) extends Piece {
  import Pawn._

  override def getPieceValue: Int = value

   def calculateLegalMove(board: Board): List[Option[Move]] = {
    val legalMove = ListBuffer[Option[Move]]()

    for(candidate <- CandidateMoveOffset ){
      val destCoord = this.position + candidate * this.alliance.direction
      if(!BoardUtil.isValidTileCoord(destCoord)){}
      else (candidate,board.tile(destCoord)) match {

        case (8,OccupiedTile(_,_))=>
        case (8,EmptyTile(c)) =>  legalMove += Some(PawnMove(board,this,c))
        case (16,OccupiedTile(_,_)) =>
        case (16,EmptyTile(c)) => if(this.isFirstMove && (BoardUtil.SeventhRank(position) && alliance.isBlack) ||
          BoardUtil.SecondRank(position) && alliance.isWhite){
          val coordBeetwen = position + 8 * alliance.direction
          if(!board.tile(coordBeetwen).isOccupied) legalMove += Some(PawnJump(board,this,c))
        }
        case (7,OccupiedTile(c,p)) => if(!(BoardUtil.columsExclusion(8)(position) && alliance.isWhite ||
        BoardUtil.columsExclusion(1)(position) && alliance.isBlack)) {
          if (p.alliance != alliance) legalMove += Some(PawnAttackMove(board,this,c,p))
        }
        case (9,OccupiedTile(c,p)) => if(!(BoardUtil.columsExclusion(1)(position) && alliance.isWhite ||
          BoardUtil.columsExclusion(8)(position) && alliance.isBlack)) {
          if (p.alliance != alliance) legalMove += Some(PawnAttackMove(board,this,c,p))
        }
        case default =>

      }
    }
    legalMove.result()
  }

  override def toString = "P"

  override def movePiece(move: Move): Pawn = Pawn(move.coordDest,move.movedPiece.alliance, isFirstMove = false)
}

object Pawn{
  private val CandidateMoveOffset = List(8,16,7,9)
  private val value = 100
}