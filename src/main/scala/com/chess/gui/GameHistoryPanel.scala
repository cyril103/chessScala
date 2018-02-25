package com.chess.gui

import javax.swing.table.DefaultTableModel

import com.chess.engine.board.{Board, Move}
import com.chess.gui.Table.MoveLog

import scala.collection.mutable.ListBuffer
import scala.swing.{BorderPanel, Dimension, ScrollPane}

class GameHistoryPanel extends BorderPanel{
  import GameHistoryPanel._

  private val model = new DataModel
  val table = new scala.swing.Table()
  table.model = model
  table.rowHeight = 15
  val scrollPane = new ScrollPane(table)
  // TODO scrollPane.columnHeaderView = table
  scrollPane.preferredSize = HISTORY_PANEL_DIMENSION
  layout += scrollPane -> BorderPanel.Position.Center
  visible = true

  private def calculateCheckAndMateHash(board: Board): String =
    if(board.currentPlayer.isInCheckMate) "#" else if(board.currentPlayer.isInCheck) "+" else ""

  def redo(board: Board, moveHistory: MoveLog): Unit = {

    var currentRow = 0
    model.clear()
    for(move <- moveHistory.getMoves){
      val moveText = move.toString
      if(move.movedPiece.alliance.isWhite){
        model.setValueAt(moveText,currentRow,0)
              }else if(move.movedPiece.alliance.isBlack){
        model.setValueAt(moveText,currentRow,1)
        currentRow += 1
      }
    }
    if(moveHistory.getMoves.nonEmpty){
      val lastMove : Move = moveHistory.getMoves(moveHistory.size - 1)
      val moveText = lastMove.toString

      if(lastMove.movedPiece.alliance.isWhite){
        model.setValueAt(moveText + calculateCheckAndMateHash(board),currentRow , 0)
      }else if(lastMove.movedPiece.alliance.isBlack){
        model.setValueAt(moveText + calculateCheckAndMateHash(board),currentRow - 1,1)
      }
    }

    val vertical = scrollPane.verticalScrollBar
    vertical.value = vertical.maximum

  }


}

object GameHistoryPanel{
  val HISTORY_PANEL_DIMENSION = new Dimension(200,800)

  private class DataModel extends DefaultTableModel{
    import DataModel._
    private val values = ListBuffer.empty[Row]

    def clear(): Unit = {
      values.clear()
      setRowCount(0)
    }

    override def getRowCount: Int = if(values == null) 0 else values.size

    override def getColumnCount: Int = NAMES.length

    override def getValueAt(row: Int, column: Int): String = {
      val currentRow = values(row)
      if(column == 0)  currentRow.whiteMove
      else if (column == 1) currentRow.blackMove
      else null
    }

    override def setValueAt(aValue: scala.Any, row: Int, column: Int): Unit = {
      var currentRow = new Row
      if(values.size <= row) values += currentRow else currentRow = values(row)
      if(column == 0){
        currentRow.whiteMove = aValue.toString
        fireTableRowsInserted(row,row)
      }else if(column == 1){
        currentRow.blackMove = aValue.toString
        fireTableCellUpdated(row,column)
      }
    }


    override def getColumnClass(i: Int): Class[_] = Move.getClass

    override def getColumnName(i: Int): String = NAMES(i)





  }

  object DataModel{
    private val NAMES = Array("White","Black")

    private class Row{
      private var _whiteMove = ""
      private var _blackMove = ""

      def whiteMove: String = _whiteMove

      def blackMove: String = _blackMove

      def whiteMove_=(move : String): Unit = _whiteMove = move

      def blackMove_=(move : String): Unit = _blackMove = move



    }
  }


}
