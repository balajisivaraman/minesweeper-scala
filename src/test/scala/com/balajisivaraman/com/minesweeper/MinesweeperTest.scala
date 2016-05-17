package com.balajisivaraman.com.minesweeper

import org.specs2.mutable._
import org.specs2.matcher.ShouldMatchers

import DataTypes._
import Minesweeper._

class MinesweeperTest extends Specification with ShouldMatchers {

  "The minesweeper game" should {
    "initiate an empty game with all hidden squares" in {
      val board = newGameBoard(3, 3, 2)
      board.flatten.forall(s: Square => s.s.equals(Hidden))
    }
  }
}
