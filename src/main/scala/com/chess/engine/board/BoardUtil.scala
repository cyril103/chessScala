package com.chess.engine.board

object BoardUtil {



  val NumOfTile = 64
  val NumTilePerRow = 8

  private val FirstColumn: Array[Boolean] = init(0)
  private val SecondColumn: Array[Boolean] = init(1)
  private val SevenColumn : Array[Boolean] = init(6)
  private val EightColumn: Array[Boolean] = init(7)



  private val AlgebraicNotation : Array[String] = Array(
    "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
    "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
    "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
    "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
    "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1"
  )



  private val PositionToCoordinate : Map[String,Int] = AlgebraicNotation.zipWithIndex.toMap

  val EightRank : Array[Boolean] = initRow(0)
  val SeventhRank : Array[Boolean] = initRow(8)
  val SixthRank : Array[Boolean] = initRow(16)
  val FifthRank : Array[Boolean] = initRow(24)
  val FourthRank : Array[Boolean] = initRow(32)
  val ThirdRank : Array[Boolean] = initRow(40)
  val SecondRank : Array[Boolean] = initRow(48)
  val FirstRank : Array[Boolean] = initRow(56)


  val columsExclusion = Map(1 -> FirstColumn,
                            2 -> SecondColumn,
                            7 -> SevenColumn,
                            8 -> EightColumn)





  private def init(i: Int): Array[Boolean] = {
    val tab: Array[Boolean] = new Array[Boolean](NumOfTile)
    var columNumber = i
    do{
      tab(columNumber) = true
      columNumber += NumTilePerRow
    }while(columNumber < NumOfTile)
    tab
  }

  private def initRow(i : Int): Array[Boolean] = {
    val tab: Array[Boolean] = new Array[Boolean](NumOfTile)
    var rowNumber = i
    do{
      tab(rowNumber) = true
      rowNumber +=1
    }while(rowNumber % NumTilePerRow != 0)
    tab
  }

  def isValidTileCoord(i: Int) : Boolean =  i >= 0 && i < NumOfTile

  def getCoordinateAtPosition(position : String) : Int =  PositionToCoordinate(position)


  def getPositionAtCoordinate(coordinate: Int): String = AlgebraicNotation(coordinate)

}
