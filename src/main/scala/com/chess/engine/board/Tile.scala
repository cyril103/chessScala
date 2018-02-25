package com.chess.engine.board

import com.chess.engine.board.Tile.{EmptyTile, OccupiedTile}
import com.chess.engine.pieces.Piece

import scala.collection.immutable.Map

trait Tile {


  def coordinate : Int

  def isOccupied : Boolean = this match{
    case EmptyTile(_) => false
    case OccupiedTile(_,_) => true
  }

  def getPiece : Option[Piece] = this match{
    case EmptyTile(_) => None
    case OccupiedTile(_,p) => Some(p)
  }

  override def toString: String = this match{
    case EmptyTile(_) => "-"
    case OccupiedTile(_,p) if p.alliance.isBlack => p.toString.toLowerCase
    case OccupiedTile(_,p) if p.alliance.isWhite => p.toString.toUpperCase
  }

}

object Tile {

  def apply(coordinate :Int, piece : Option[Piece]): Tile = piece match {
    case None => EmptyTileCache(coordinate)
    case Some(p) => OccupiedTile(coordinate,p)
  }

  private val EmptyTileCache : Map[Int, EmptyTile] = List.tabulate(BoardUtil.NumOfTile)(i => (i,EmptyTile(i))).toMap

  case class EmptyTile (coordinate : Int ) extends Tile

  case class OccupiedTile (coordinate :Int, pieceOnTile : Piece) extends Tile


}