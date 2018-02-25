package com.chess.engine.player

import com.chess.engine.{Alliance, BLACK, WHITE}
import com.chess.engine.board._
import com.chess.engine.pieces.{King, Piece, Rook}

sealed abstract  class Player(val board : Board,
                              _legalMoves : List[Option[Move]],
                       val opponentMoves : List[Option[Move]]) {

  def legalMoves: List[Option[Move]] = _legalMoves ++ calculateKingCastle(_legalMoves,opponentMoves)



  protected def playerKing : King = establishKing().get.asInstanceOf[King]

  def establishKing(): Option[Piece] = getActivePieces.find{
    case _:King => true
    case _ => false}

  def isLegalMove(move : Move): Boolean = legalMoves.contains(Option(move))

  def getActivePieces : List[Piece]
  def getAlliance : Alliance
  def getOpponent : Player
  protected def calculateKingCastle(playerLegals : List[Option[Move]], opponentsLegals : List[Option[Move]]) : List[Option[Move]]

  def  isInCheck: Boolean = Player.calculateAttackOnTile(playerKing.position, opponentMoves).nonEmpty

  protected def hasEscape : Boolean = {
    for(move <- legalMoves){
      val transition : MoveTransition = makeMove(move.get)
      if(transition.moveStatus.isDone) return true
    }
    false
  }


  def isInCheckMate: Boolean = isInCheck && !hasEscape

  def isInStaleMate: Boolean = !isInCheck && !hasEscape
  // TODO
  def isCastled : Boolean = false

  def makeMove(move : Move) : MoveTransition = {
    if(!isLegalMove(move)){
      return MoveTransition(this.board,move,MoveStatus.ILLEGAL_MOVE)
    }
    val transitionBoard = move.execute()
    val kingAttacks = Player.calculateAttackOnTile(transitionBoard.currentPlayer.getOpponent.playerKing.position,
      transitionBoard.currentPlayer.legalMoves)
    if(kingAttacks.nonEmpty) return MoveTransition(this.board, move, MoveStatus.LEAVES_PLAYER_IN_CHECK)

     MoveTransition(transitionBoard, move , MoveStatus.DONE)
  }



}

case class WhitePlayer(override val board: Board,
                       whiteMoves : List[Option[Move]],
                       blackMoves : List[Option[Move]]) extends Player(board,whiteMoves,blackMoves){

  legalMoves.foreach(om => if(om.get.isCastlingMove) println(om.get))

  def getActivePieces: List[Piece] = board.getWhitePieces

  def getAlliance: WHITE.type = WHITE

  def getOpponent: BlackPlayer = board.blackPlayer

  override def toString: String = "White"

  override protected def calculateKingCastle(playerLegals: List[Option[Move]], opponentsLegals: List[Option[Move]]): List[Option[Move]] = {
    var kingCastle: List[Option[Move]] = List()

    if(playerKing.isFirstMove && !isInCheck){
      //kingSideCastle
      if(!board.tile(61).isOccupied && !board.tile(62).isOccupied){
        val rookTile = board.tile(63)
        val isRook = rookTile.getPiece.get match {case _:Rook => true
                                                  case _ => false}

        if(rookTile.isOccupied && rookTile.getPiece.get.isFirstMove){

          if(Player.calculateAttackOnTile(61,opponentsLegals).isEmpty &&
            Player.calculateAttackOnTile(62,opponentsLegals).isEmpty &&
             isRook) kingCastle ::= Option(KingSideCastleMove(board,
            playerKing,
            62,
            rookTile.getPiece.get.asInstanceOf[Rook],
            rookTile.coordinate,
            61))

        }
      }

      // QueenSideCastle
      if(!board.tile(59).isOccupied && !board.tile(58).isOccupied && !board.tile(57).isOccupied){
        val rookTile: Tile = board.tile(56)
        val isRook: Boolean = rookTile.getPiece.get match {case _:Rook => true
        case _ => false}

        if(rookTile.isOccupied && rookTile.getPiece.get.isFirstMove){
          if(Player.calculateAttackOnTile(59,opponentsLegals).isEmpty &&
             Player.calculateAttackOnTile(58,opponentsLegals).isEmpty &&
            isRook)
          kingCastle ::= Option(QueenSideCastleMove(board,
            playerKing,
            58,
            rookTile.getPiece.get.asInstanceOf[Rook],
            rookTile.coordinate,
            59))
        }
      }
    }
    kingCastle
  }
}

case class BlackPlayer(override val board: Board,
                       whiteMoves : List[Option[Move]],
                       blackMoves : List[Option[Move]]) extends  Player(board,blackMoves,whiteMoves){
  def getActivePieces: List[Piece] = board.getBlackPieces

  def getAlliance: BLACK.type = BLACK

  def getOpponent: WhitePlayer = board.whitePlayer

  override def toString: String = "Black"

  override protected def calculateKingCastle(playerLegals: List[Option[Move]], opponentsLegals: List[Option[Move]]): List[Option[Move]] = {
    var kingCastle: List[Option[Move]] = List()



    if(playerKing.isFirstMove && !isInCheck){
      // black kingSideCastle
      if(!board.tile(5).isOccupied && !board.tile(6).isOccupied){
        val rookTile: Tile = board.tile(7)
        val isRook: Boolean = rookTile.getPiece.get match {case _:Rook => true
        case _ => false}

        if(rookTile.isOccupied && rookTile.getPiece.get.isFirstMove){

          if(Player.calculateAttackOnTile(5,opponentsLegals).isEmpty &&
             Player.calculateAttackOnTile(6,opponentsLegals).isEmpty &&
            isRook) kingCastle ::= Option(KingSideCastleMove(board,
            playerKing,
            6,
            rookTile.getPiece.get.asInstanceOf[Rook],
            7,
            5))

        }
      }

      // QueenSideCastle
      if(!board.tile(1).isOccupied && !board.tile(2).isOccupied && !board.tile(3).isOccupied){
        val rookTile: Tile = board.tile(0)
        val isRook: Boolean = rookTile.getPiece.get match {case _:Rook => true
        case _ => false}

        if(rookTile.isOccupied && rookTile.getPiece.get.isFirstMove){
          if(Player.calculateAttackOnTile(2,opponentsLegals).isEmpty &&
             Player.calculateAttackOnTile(3,opponentsLegals).isEmpty &&
            isRook)
            kingCastle ::= Option(QueenSideCastleMove(board,
              playerKing,
              2,
              rookTile.getPiece.get.asInstanceOf[Rook],
              0,
              3))
        }
      }
    }
    kingCastle
  }
}

object Player{

  def calculateAttackOnTile(position: Int, moves: List[Option[Move]]) : Seq[Option[Move]] =
    moves.filter(move => move.get.coordDest == position)

}
