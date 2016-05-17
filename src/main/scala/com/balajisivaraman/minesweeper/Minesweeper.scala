package com.balajisivaraman.minesweeper

object DataTypes {

  case class Position(x: Int, y: Int)

  sealed trait State
  case object Hidden extends State
  case object Revealed extends State
  case object Flagged extends State

  sealed trait Contents
  case object Empty extends Contents
  case object Mine extends Contents
  case class Number(i: Int) extends Contents

  case class Square(p: Position, c: Contents, s: State)
  type Row = List[Square]
  type Board = List[Row]

}

import DataTypes._

object Minesweeper {

  def newGameBoard(w: Int, h: Int, n: Int): Board =
    (for(i <- 0 until w) yield {
      (for(j <- 0 until h)
      yield {
        Square(Position(i,j), Empty, Hidden)
      }).toList
    }).toList

  def showSquare(s: Square): String =
    s.s match {
      case Hidden   => "X"
      case Revealed => showContents(s)
      case Flagged  => "F"
    }

  def showContents(s: Square): String =
    s.c match {
      case Empty     => " "
      case Mine      => "M"
      case Number(i) => i.toString
    }

  def showRow(r: Row): List[String] = r map showSquare

  def showBoard(b: Board): List[List[String]] = b map showRow

  def openSquare(b: Board, p: Position): Option[Board] = {
    val square = b(p.x)(p.y)
    square.c match {
      case Mine      => None
      case Number(i) => Some(setSquareContents(b, p, square.c, Revealed))
      case Empty     => Some(setSquareContents(b, p, square.c, Revealed))
    }
  }

  def flagSquare(b: Board, p: Position): Board = setSquareContents(b, p, b(p.x)(p.y).c, Flagged)

  private def setSquareContents(b: Board, p: Position, c: Contents, s: State): Board =
    b.updated(p.x,b(p.x).updated(p.y,Square(p,c,s)))

  def setMine(b: Board, p: Position): Board = setSquareContents(b, p, Mine, Hidden)

  def setNumber(b: Board, p: Position): Board = setSquareContents(b, p, Number(0), Hidden)

}

import Minesweeper._

object MinesweeperApp extends App {

  var b = newGameBoard(3, 3, 2)
  b = setMine(b,Position(0,2))
  b = setMine(b,Position(1,1))
  b = setNumber(b, Position(0,0))
  b = setNumber(b, Position(0,1))
  b = setNumber(b, Position(1,0))
  b = setNumber(b, Position(1,2))
  b = setNumber(b, Position(2,0))
  b = setNumber(b, Position(2,1))
  b = setNumber(b, Position(2,2))
  var ob = openSquare(b, Position(0,0))
  b = endGame(ob)
  ob = openSquare(b, Position(0,1))
  b = endGame(ob)
  b = flagSquare(b, Position(0,2))
  ob = openSquare(b, Position())
  showBoard(b) map println

  //Game ending scenario - Mine Openedb = newGameBoard(3, 3, 2)
  b = setMine(b,Position(0,2))
  b = setMine(b,Position(1,1))
  b = setNumber(b, Position(0,0))
  b = setNumber(b, Position(0,1))
  b = setNumber(b, Position(1,0))
  b = setNumber(b, Position(1,2))
  b = setNumber(b, Position(2,0))
  b = setNumber(b, Position(2,1))
  b = setNumber(b, Position(2,2))
  val ob1 = openSquare(b, Position(1,1))
  endGame(ob)
  def endGame(ob: Option[Board]): Board = ob.getOrElse(throw new IllegalStateException("Oops! You stepped on a mine!"))

}
