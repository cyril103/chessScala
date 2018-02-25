package com.chess.engine.board

import com.chess.engine.pieces.{Pawn, Piece, Rook}

 sealed trait Move {

  def coordDest : Int
  def movedPiece : Piece
  def board : Board
  def isFirstmove : Boolean = movedPiece.isFirstMove

  def getCurrentCoordinate : Int = movedPiece.position

  def isAttack : Boolean = false

  def isCastlingMove : Boolean = false

  def getAttackedPiece : Option[Piece] = None

  def execute(): Board = {
    val builder = new Board.Builder()

    board.currentPlayer.getActivePieces.foreach(piece => if(this.movedPiece != piece) builder.setPiece(piece))
    board.currentPlayer.getOpponent.getActivePieces.foreach(builder.setPiece)

    builder.setPiece(this.movedPiece.movePiece(this))
    builder.setMoveMaker(this.board.currentPlayer.getOpponent.getAlliance)

    builder.build()

  }

}

trait MoveWithAttack extends Move{
  def pieceTarget : Piece

  override def isAttack: Boolean = true

  override def getAttackedPiece: Option[Piece] = Option(pieceTarget)
}



case class MajorMove(board : Board, movedPiece: Piece, coordDest :Int ) extends Move{

  override def toString: String = movedPiece.toString + BoardUtil.getPositionAtCoordinate(this.coordDest)
}


case class AttackMove(board: Board, movedPiece: Piece, coordDest : Int , pieceTarget : Piece ) extends MoveWithAttack{

  override def toString: String = movedPiece.toString + "x" +
    BoardUtil.getPositionAtCoordinate(this.coordDest)
}


case class PawnMove(board : Board, movedPiece: Piece, coordDest :Int ) extends Move{

  override def toString: String = BoardUtil.getPositionAtCoordinate(this.coordDest)
}



case class PawnAttackMove( board : Board,
                           movedPiece: Piece,
                           coordDest :Int,
                           pieceTarget : Piece ) extends MoveWithAttack{

  override def toString: String = BoardUtil.getPositionAtCoordinate(this.movedPiece.position).substring(0, 1) + "x" +
    BoardUtil.getPositionAtCoordinate(this.coordDest)
}



case class PawnEnPassantAttackMove( board : Board,
                          movedPiece: Piece,
                          coordDest :Int,
                          pieceTarget : Piece ) extends MoveWithAttack



case class PawnJump(board : Board, movedPiece: Piece, coordDest :Int ) extends Move{

  override def execute(): Board = {

    val builder = new Board.Builder()

    board.currentPlayer.getActivePieces.foreach(piece => if(this.movedPiece != piece) builder.setPiece(piece))
    board.currentPlayer.getOpponent.getActivePieces.foreach(builder.setPiece)
    val movePawn : Pawn = movedPiece match {case _: Pawn => this.movedPiece.movePiece(this).asInstanceOf[Pawn]}
    builder.setPiece(movePawn)
    builder.setEnPassantPawn(movePawn)
    builder.setMoveMaker(this.board.currentPlayer.getOpponent.getAlliance)
    builder.build()
  }

  override def toString: String = BoardUtil.getPositionAtCoordinate(this.coordDest)
}



trait CastleMove  extends Move{
  def castleRook :Rook
  def castleRookStart : Int
  def castleRookDestination : Int

  override def isCastlingMove: Boolean = true

  override def execute(): Board = {
    val builder = new Board.Builder()

    board.currentPlayer.getActivePieces.foreach(piece => if(this.movedPiece != piece && castleRook != piece) builder.setPiece(piece))
    board.currentPlayer.getOpponent.getActivePieces.foreach(builder.setPiece)

    builder.setPiece(this.movedPiece.movePiece(this))
    builder.setPiece(Rook(castleRookDestination,castleRook.alliance,isFirstMove = false))
    builder.setMoveMaker(this.board.currentPlayer.getOpponent.getAlliance)

    builder.build()

  }
}


case class KingSideCastleMove(board : Board,
                              movedPiece: Piece,
                              coordDest :Int,
                              castleRook : Rook,
                              castleRookStart : Int,
                              castleRookDestination : Int) extends CastleMove{
  override def toString: String = "O-O"
}



case class QueenSideCastleMove(board : Board,
                               movedPiece: Piece,
                               coordDest :Int,
                               castleRook : Rook,
                               castleRookStart : Int,
                               castleRookDestination : Int) extends CastleMove{
  override def toString: String = "O-O-O"
}



case object NullMove extends Move {
  override def coordDest : Nothing = throw new RuntimeException("not valid move")

  override def movedPiece: Nothing = throw new RuntimeException("not valid move")

  override def board: Nothing = throw new RuntimeException("not valid move")

  override def execute(): Nothing = throw new RuntimeException("cannot execute the null move")
}

object Move{

  def apply (board: Board, currentCoordinate : Int, destinationCoordinate : Int) : Move = {

    for(move  <- board.allLegalMoves){
      if(move.get.getCurrentCoordinate == currentCoordinate &&
      move.get.coordDest == destinationCoordinate) return move.get
    }
     NullMove
  }


  }



