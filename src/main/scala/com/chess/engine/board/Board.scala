package com.chess.engine.board

import com.chess.engine.board.Tile.OccupiedTile
import com.chess.engine.pieces._
import com.chess.engine.player.{BlackPlayer, Player, WhitePlayer}
import com.chess.engine.{Alliance, BLACK, WHITE}

import scala.collection.mutable

class Board private (builder : Board.Builder){


  private val gameBoard : List[Tile] = List.tabulate(BoardUtil.NumOfTile)(i => Tile(i,builder.boardConfig.get(i)))
  private val whitePiece = calculatePieces(gameBoard,WHITE)
  private val blackPiece = calculatePieces(gameBoard,BLACK)

  val whiteStandardLegalMoves: List[Option[Move]] = whitePiece.flatMap(p => p.calculateLegalMove(this))
  val blackStandardLegalMoves: List[Option[Move]] = blackPiece.flatMap(p => p.calculateLegalMove(this))

  val  whitePlayer : WhitePlayer = WhitePlayer(this,whiteStandardLegalMoves,blackStandardLegalMoves)
  val  blackPlayer : BlackPlayer = BlackPlayer(this,whiteStandardLegalMoves,blackStandardLegalMoves)



  val allLegalMoves: List[Option[Move]] = whitePlayer.legalMoves ++ blackPlayer.legalMoves



  val currentPlayer : Player = builder.nextMoveMaker.choosePlayer(this.whitePlayer,this.blackPlayer)


  def tile(coord :Int) : Tile = gameBoard(coord)

  def getBlackPieces: List[Piece] = blackPiece
  def getWhitePieces: List[Piece] = whitePiece

  private def calculatePieces(gameBoard : List[Tile], alliance : Alliance): List[Piece] = gameBoard.filter{
  case OccupiedTile(_,p) if p.alliance == alliance => true
  case _ => false
    }
    .map{case OccupiedTile(_,p) => p}

  override def toString: String = {
    val builder = new mutable.StringBuilder()
    for(i <- 0 until BoardUtil.NumOfTile){
      val tileText = gameBoard(i).toString
      builder.append(String.format("%3s",tileText))
      if((i +1) % BoardUtil.NumTilePerRow == 0)  builder.append("\n")
    }
    builder.toString()
  }






}

object Board{

  def createStandardBoard() : Board = {
    val builder = new Builder
    // black layout
    builder.setPiece(Rook(0,BLACK))
    builder.setPiece(Knight(1,BLACK))
    builder.setPiece(Bishop(2,BLACK))
    builder.setPiece(Queen(3,BLACK))
    builder.setPiece(King(4,BLACK))
    builder.setPiece(Bishop(5,BLACK))
    builder.setPiece(Knight(6,BLACK))
    builder.setPiece(Rook(7,BLACK))
    builder.setPiece(Pawn(8,BLACK))
    builder.setPiece(Pawn(9,BLACK))
    builder.setPiece(Pawn(10,BLACK))
    builder.setPiece(Pawn(11,BLACK))
    builder.setPiece(Pawn(12,BLACK))
    builder.setPiece(Pawn(13,BLACK))
    builder.setPiece(Pawn(14,BLACK))
    builder.setPiece(Pawn(15,BLACK))
    // white layout

    builder.setPiece(Pawn(48,WHITE))
    builder.setPiece(Pawn(49,WHITE))
    builder.setPiece(Pawn(50,WHITE))
    builder.setPiece(Pawn(51,WHITE))
    builder.setPiece(Pawn(52,WHITE))
    builder.setPiece(Pawn(53,WHITE))
    builder.setPiece(Pawn(54,WHITE))
    builder.setPiece(Pawn(55,WHITE))
    builder.setPiece(Rook(56,WHITE))
    builder.setPiece(Knight(57,WHITE))
    builder.setPiece(Bishop(58,WHITE))
    builder.setPiece(Queen(59,WHITE))
    builder.setPiece(King(60,WHITE))
    builder.setPiece(Bishop(61,WHITE))
    builder.setPiece(Knight(62,WHITE))
    builder.setPiece(Rook(63,WHITE))

    builder.setMoveMaker(WHITE)

    builder.build()

  }

  class Builder{

    private var enPassantPawn : Pawn = _

    def setEnPassantPawn(enPassntPawn: Pawn): Unit = this.enPassantPawn = enPassntPawn


    val boardConfig: mutable.Map[Int, Piece] = mutable.Map[Int,Piece]()
    private var _nextMoveMaker : Alliance = _

    def build() = new Board(this)

    def setPiece(piece : Piece) : Builder = {
      boardConfig += (piece.position -> piece)
      this
    }

    def setMoveMaker(nextMoveMaker : Alliance) : Builder = {
      this._nextMoveMaker = nextMoveMaker
      this
    }

    def nextMoveMaker : Alliance = _nextMoveMaker

  }
}
