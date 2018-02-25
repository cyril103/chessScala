package com.chess.gui

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{File, IOException}
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, SwingUtilities}

import com.chess.engine.board._
import com.chess.engine.pieces.{King, Piece}

import scala.collection.mutable.ListBuffer
import scala.swing.event.{ButtonClicked, MouseClicked}
import scala.swing.{Action, BorderPanel, Button, CheckMenuItem, Dimension, GridPanel, MainFrame, Menu, MenuBar, MenuItem, Separator}


class Table {
  private val lightTileColor: Color = Color.decode("#FFFACD")
  private val darkTileColor: Color = Color.decode("#593E1A")
  private var sourceTile : Tile = _
  private var destinationTile : Tile = _
  private var humanMovePiece : Option[Piece] = None
  private var boardDirection : BoarDirection = NORMAL
  private var hightLightLegalsMoves : Boolean = false

  private var chessBoard : Board = Board.createStandardBoard()



  import Table._
  private val gameFrame : MainFrame =  new MainFrame
  val tableMenuBar: MenuBar =  createTableMenuBar
  gameFrame.menuBar = tableMenuBar

  gameFrame.size = OUTTER_FRAME_DIMENSION
  private val gameHistoryPanel = new GameHistoryPanel
  private val takenPiecePanel = new TakenPiecePanel
  val boardPanel = new BoardPanel
  private val moveLog = new MoveLog

  gameFrame.contents = new BorderPanel{
    layout += boardPanel -> BorderPanel.Position.Center
    layout += takenPiecePanel -> BorderPanel.Position.West
    layout += gameHistoryPanel -> BorderPanel.Position.East
  }
  gameFrame.title = "ScalaChess"
  gameFrame.visible = true




  private def createTableMenuBar : MenuBar = {
    val tableMenuBar = new MenuBar
    tableMenuBar.contents.append(createFileMenu(),preferenceMenu())
    tableMenuBar
  }
  private def createFileMenu() = {
    val fileMenu  : Menu = new Menu("File")

    val openPGN = new MenuItem(Action("Load PGN File"){ println("open up that pgn file")})
    fileMenu.contents += openPGN

    val exitMenu = new MenuItem(Action("Exit") { sys.exit(0)})
    fileMenu.contents += exitMenu

    fileMenu
  }



  private def preferenceMenu() = {
    val preferenceMenu = new Menu("Preferences")
    val flipBoardMenuItem = new MenuItem(Action("Flip Board"){
     boardDirection = boardDirection.opposite
      boardPanel.drawBoard(chessBoard)
    })
    preferenceMenu.contents += flipBoardMenuItem

    val highlight = new CheckMenuItem("HightLight Legals "){
      listenTo(this)
      reactions += {
        case _ : ButtonClicked =>  hightLightLegalsMoves = selected
      }

    }

    preferenceMenu.contents += new Separator()
    preferenceMenu.contents += highlight

    preferenceMenu
  }

   class BoardPanel extends GridPanel(8,8){



     val boardTiles : List[TilePanel] = List.tabulate(BoardUtil.NumOfTile)(i => new TilePanel(this,i))
    boardTiles.foreach(tile => contents += tile)
    preferredSize = BOARD_PANEL_DIMENSION

    validate()

     def drawBoard(board : Board): Unit = {
       contents.clear()
       boardDirection.traverse(boardTiles).foreach{ tilePanel =>
         tilePanel.drawTile(board)
         contents += tilePanel

       }
       validate()
       repaint()
     }


  }

   class TilePanel(val boardPanel : BoardPanel, val tileId : Int) extends Button {


     preferredSize = TILE_PANEL_DIMENSION
    assignTileColor()
    assignTilePieceIcon(chessBoard)

     listenTo(mouse.clicks)
     reactions += {
       case e : MouseClicked   => e.peer.getButton match {
         // Left
         case 1 =>
           if(sourceTile == null){
             sourceTile = chessBoard.tile(tileId)
             humanMovePiece = sourceTile.getPiece
             if(humanMovePiece.isEmpty) sourceTile = null
           }else {
             destinationTile = chessBoard.tile(tileId)
             val move : Move = Move.apply(chessBoard,sourceTile.coordinate,destinationTile.coordinate)
             val transition = chessBoard.currentPlayer.makeMove(move)
             if (transition.moveStatus.isDone){
               chessBoard = transition.transitionBoard
               moveLog.addMoves(move)
               }
             sourceTile = null
             destinationTile = null
             humanMovePiece = None
           }
           SwingUtilities.invokeLater{() =>
             gameHistoryPanel.redo(chessBoard,moveLog)
             takenPiecePanel.redo(moveLog)
             boardPanel.drawBoard(chessBoard)}
         // Right
         case 3 =>
           sourceTile = null
           destinationTile = null
           humanMovePiece  = None

         // Other
         case _ =>
       }

     }




    validate()

     def drawTile(board: Board): Unit = {
       assignTileColor()
       assignTilePieceIcon(board)
       highLightLegals(board)
       validate()
       repaint()
     }

     private def assignTilePieceIcon(board :Board) : Unit = {
       icon = null

       if(board.tile(tileId).isOccupied){


         try {
           val image: BufferedImage =
             ImageIO.read(new File(defaultPieceImagePath + board.tile(tileId).getPiece.get.alliance.toString.substring(0, 1) +
               board.tile(tileId).getPiece.get.toString + ".gif"))


           this.icon = new ImageIcon(image)


         }catch {
           case e : IOException => e.printStackTrace()
         }
       }

     }
     private def pieceLegalMovesBoard(board: Board) : List[Move] = {
       val kingCastel = board.currentPlayer.legalMoves.map(_.get).filter(_.isCastlingMove)
       if(humanMovePiece.nonEmpty &&
         humanMovePiece.get.alliance == board.currentPlayer.getAlliance
       ) humanMovePiece.get.calculateLegalMove(board).map(_.get) ++ {if(humanMovePiece.get.isInstanceOf[King])kingCastel
       else Nil} else List()
     }

     private def highLightLegals(board: Board) : Unit = {
       if(hightLightLegalsMoves){
         pieceLegalMovesBoard(board).foreach{ move : Move =>
           if(move.coordDest == tileId) try{
             icon = new ImageIcon(ImageIO.read(new File("art/misc/green_dot.png")))
           }catch {
             case e : IOException => e.printStackTrace()
           }

         }
       }
     }

     private def assignTileColor(): Unit = {
       if(BoardUtil.EightRank(tileId) ||
       BoardUtil.SixthRank(tileId) ||
       BoardUtil.FourthRank(tileId) ||
       BoardUtil.SecondRank(tileId)) {
         background = if(tileId % 2 == 0)lightTileColor else  darkTileColor
       }else if(
         BoardUtil.SeventhRank(tileId) ||
       BoardUtil.FifthRank(tileId) ||
       BoardUtil.ThirdRank(tileId) ||
       BoardUtil.FirstRank(tileId)){
         background = if(tileId % 2 !=0) lightTileColor else darkTileColor
       }
     }
  }

 sealed trait BoarDirection{
    def traverse(boardTile : List[TilePanel]) : List[TilePanel]
    def opposite : BoarDirection
  }

  case object  NORMAL extends BoarDirection{

    override def traverse(boardTile: List[TilePanel]): List[TilePanel] = boardTile

    override def opposite: BoarDirection = FLIPPED
  }

  case object FLIPPED extends BoarDirection{
    override def traverse(boardTile: List[TilePanel]): List[TilePanel] = boardTile.reverse

    override def opposite: BoarDirection = NORMAL
  }

}


object Table{

  private val OUTTER_FRAME_DIMENSION = new Dimension(800,800)
  private val BOARD_PANEL_DIMENSION = new Dimension(800,800)
  private val TILE_PANEL_DIMENSION = new Dimension(20,20)
  private val defaultPieceImagePath = "art/simple/"

  class MoveLog{

    private val moves = ListBuffer.empty[Move]

    def getMoves: List[Move] = this.moves.result()

    def addMoves(move : Move): Unit = moves += move

    def size: Int = moves.size

    def clear(): Unit = moves.clear()

    def remove(move : Move): Boolean = (moves -= move).size < moves.size

    def remove(index : Int): Move = moves.remove(index)
  }
}
