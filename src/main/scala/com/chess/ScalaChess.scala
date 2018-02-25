package com.chess

import com.chess.engine.board.Board
import com.chess.gui.Table

object ScalaChess {

  def main(args: Array[String]): Unit = {


    val board = Board.createStandardBoard()

    println(board)



    new Table

  }
}
