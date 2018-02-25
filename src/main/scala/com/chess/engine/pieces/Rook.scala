package com.chess.engine.pieces

import com.chess.engine.Alliance
import com.chess.engine.board.Tile.{EmptyTile, OccupiedTile}
import com.chess.engine.board._

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

case class Rook(position : Int, alliance : Alliance, isFirstMove : Boolean = true )extends Piece {
  import Rook._

  override def getPieceValue: Int = value

  override def calculateLegalMove(board: Board): List[Option[Move]] = {
    val legalMoves = ListBuffer[Option[Move]]()
    for(candidateCoordinateOffset <- CandidateMoveOffset){
      var candidateDestinationCoordinate = this.position

      breakable {
        while(BoardUtil.isValidTileCoord(candidateDestinationCoordinate)){
          if(isExclusion(1)(candidateDestinationCoordinate,candidateCoordinateOffset) ||
            isExclusion(8)(candidateDestinationCoordinate,candidateCoordinateOffset)){
            break()
          }

          candidateDestinationCoordinate += candidateCoordinateOffset

          if(BoardUtil.isValidTileCoord(candidateDestinationCoordinate)){
            val candidateDestinationTile = board.tile(candidateDestinationCoordinate)
            candidateDestinationTile match{
              case EmptyTile(c) => legalMoves += Some(MajorMove(board,this,c))
              case OccupiedTile(c,p) =>
                if(this.alliance != p.alliance) legalMoves += Some(AttackMove(board,this,c,p))
                break()
            }
          }
        }
      }
    }
    legalMoves.result()
  }

  override def toString = "R"

  override def movePiece(move: Move): Rook = Rook(move.coordDest,move.movedPiece.alliance, isFirstMove = false)
}

object Rook {
  private val CandidateMoveOffset = List(-1,1,8,-8)
  private val value = 500

  private def isExclusion(column: Int)(currentPos :Int, offset :Int) =  {
    val offsetSet = column match{
      case 1 => Set[Int](-1)
      case 8 => Set[Int](1)
    }
    BoardUtil.columsExclusion (column) (currentPos) && offsetSet.contains (offset)
  }
}
