package com.test

import com.chess.engine.board.Board
import org.scalatest.FunSuite

class TestBoard extends FunSuite {

  test("initial Board") {
    val board = Board.createStandardBoard()

    assert(board.currentPlayer.legalMoves.size == 20)
    assert(board.currentPlayer.getOpponent.legalMoves.size == 20)
    assert(!board.currentPlayer.isInCheck)
    assert(!board.currentPlayer.isInCheckMate)
    assert(!board.currentPlayer.isCastled)
    //assert(board.currentPlayer.isKingSideCastleCapable)
    //assert(board.currentPlayer.isQueenSideCastleCapable)
    assert(board.currentPlayer == board.whitePlayer)
    assert(board.currentPlayer.getOpponent == board.blackPlayer)
    assert(!board.currentPlayer.getOpponent.isInCheck)
    assert(!board.currentPlayer.getOpponent.isInCheckMate)
    assert(!board.currentPlayer.getOpponent.isCastled)
    //assert(board.currentPlayer().getOpponent().isKingSideCastleCapable())
    //assert(board.currentPlayer().getOpponent().isQueenSideCastleCapable())
    assert(board.whitePlayer.toString().equals("White"))
    assert(board.blackPlayer.toString().equals("Black"))

    val allPieces = board.getBlackPieces ++ board.getWhitePieces
    val allMoves = List.concat(board.whitePlayer.legalMoves, board.blackPlayer.legalMoves)
    for( Some(move) <- allMoves) {
      assert(!move.isAttack)
      assert(!move.isCastlingMove)
      //assert(MoveUtils.exchangeScore(move) == 1)
    }

    assert(allMoves.size == 40)
    assert(allPieces.size == 32)
    //assert(!BoardUtils.isEndGame(board))
    //assert(!BoardUtils.isThreatenedBoardImmediate(board))
    //assert(StandardBoardEvaluator.get().evaluate(board, 0), 0)
    assert(board.tile(35).getPiece.isEmpty)
    assert(board.tile(35).coordinate == 35)




  }

}
