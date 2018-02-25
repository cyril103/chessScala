package com.chess.gui

import java.awt.Color
import java.io.{File, IOException}
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import javax.swing.border.EtchedBorder

import com.chess.engine.pieces.Piece
import com.chess.gui.Table.MoveLog

import scala.collection.mutable.ListBuffer
import scala.swing.{BorderPanel, Dimension, GridPanel, Label}

class TakenPiecePanel extends BorderPanel{
  import TakenPiecePanel._

  background = PANEL_COLOR
  border = PANEL_BORDER
  private val northPanel = new GridPanel(8,2)
  private val southPanel = new GridPanel(8,2)
  northPanel.background = PANEL_COLOR
  southPanel.background = PANEL_COLOR
  this.layout += northPanel -> BorderPanel.Position.North
  this.layout += southPanel -> BorderPanel.Position.South
  preferredSize = TAKEN_PIECES_DIMENSION

  def redo(moveLog: MoveLog): Unit = {
    southPanel.contents.clear()
    northPanel.contents.clear()

    val whiteTakenPieces = ListBuffer.empty[Piece]
    val blackTakenPieces = ListBuffer.empty[Piece]

    moveLog.getMoves.foreach{ move =>
      if(move.isAttack){
        val takenPiece = move.getAttackedPiece.get
        if(takenPiece.alliance.isWhite) whiteTakenPieces += takenPiece
        else if(takenPiece.alliance.isBlack) blackTakenPieces += takenPiece
        else throw new RuntimeException("Should not")
      }
    }
    whiteTakenPieces.sortWith(_.getPieceValue > _.getPieceValue)
    blackTakenPieces.sortWith(_.getPieceValue > _.getPieceValue)

    for(takenPiece <- whiteTakenPieces.result()){
      try{
        val image = ImageIO.read(new File("art/simple/" + takenPiece.alliance.toString.substring(0,1) + takenPiece.toString + ".gif"))
        val icon: ImageIcon = new ImageIcon(image)
        val imageLabel = new Label()
        imageLabel.icon = icon
        northPanel.contents += imageLabel

      }catch {
        case e : IOException => e.printStackTrace()
      }
    }

    for(takenPiece <- blackTakenPieces.result()){
      try{
        val image = ImageIO.read(new File("art/simple/" + takenPiece.alliance.toString.substring(0,1) + takenPiece.toString +".gif"))
        val icon: ImageIcon = new ImageIcon(image)

        val imageLabel = new Label()
        imageLabel.icon = icon
        southPanel.contents += imageLabel

      }catch {
        case e : IOException => e.printStackTrace()
      }
    }
    validate()
  }




}

object TakenPiecePanel{

  private val PANEL_BORDER = new EtchedBorder(EtchedBorder.RAISED)
  private val PANEL_COLOR = Color.decode("0xFDF5E6")
  private val TAKEN_PIECES_DIMENSION = new Dimension(100,200)

}
