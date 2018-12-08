package tictactoe

import shapeless.{::, =:!=, HList, HNil}
import shapeless.syntax._
import tictactoe.Cells.{Cell, _}
import tictactoe.GameOutcome.Aux
import tictactoe.ListOps.{ForallF, HasF}
import tictactoe.Outcome.{Draw, InPlay, Outcome, Winner}
import tictactoe.PlayerAt.PlayerAt
import tictactoe.Players._
import tictactoe.RowOutcome.{IsEmpty, IsNotP1, MovesOutcome, RowOutcome}
import tictactoe.Turn.NextTurn

/*
Tonny Moris's Tick-Tac-Toe challenge
https://github.com/data61/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown
 */

object Players {

  sealed trait MaybePlayer

  sealed trait Player extends MaybePlayer

  object P1 extends Player

  object P2 extends Player

  object Empty extends MaybePlayer

}


object Turn {

  sealed trait NextTurn[Moves <: HList, P <: Player]

  implicit val firstTurn: NextTurn[HNil, P1.type] = null

  implicit def firstAfterSecond[H, T <: HList](implicit ev: NextTurn[T, P2.type]): NextTurn[H :: T, P1.type] = null

  implicit def secondAfterFirst[H, T <: HList](implicit ev: NextTurn[T, P1.type]): NextTurn[H :: T, P2.type] = null
}

object Cells {

  sealed trait Cell

  object NW extends Cell

  object N extends Cell

  object NE extends Cell

  object W extends Cell

  object C extends Cell

  object E extends Cell

  object SW extends Cell

  object S extends Cell

  object SE extends Cell

  type  ALL_ROWS_TO_CHECK = (NW.type :: N.type :: NE.type :: HNil) :: (W.type :: C.type :: E.type :: HNil) :: (SW.type :: S.type :: SE.type :: HNil) ::
    (NW.type :: W.type :: SW.type :: HNil) :: (N.type :: C.type :: S.type :: HNil) :: (NE.type :: E.type :: SE.type :: HNil) ::
    (NW.type :: C.type :: SE.type :: HNil) :: (NE.type :: C.type :: SW.type :: HNil) :: HNil

}

object PlayerAt {

  sealed trait PlayerAt[Moves <: HList, C <: Cell] {
    type Res <: MaybePlayer
  }

  type Aux[Moves <: HList, C <: Cell, P <: MaybePlayer] = PlayerAt[Moves, C] {
    type Res = P
  }

  implicit def noPlayerAtEmptyBoard[C <: Cell]: Aux[HNil, C, Empty.type] = null

  implicit def occupied[C <: Cell, T <: HList, P <: Player](implicit ev: NextTurn[T, P]): Aux[C :: T, C, P] = null

  implicit def checkOther[C <: Cell, C1 <: Cell, T <: HList, P <: MaybePlayer](implicit notThisCell: C1 =:!= C, result: Aux[T, C, P]): Aux[C1 :: T, C, P] = null
}

object Projection {

  sealed trait Projection[Moves <: HList, CELLS <: HList] {
    type Res <: HList
  }

  type Aux[Moves <: HList, CELLS <: HList, R <: HList] = Projection[Moves, CELLS] {
    type Res = R
  }

  implicit def emptyProj[Moves <: HList]: Aux[Moves, HNil, HNil] = null

  implicit def consProj[Moves <: HList, C <: Cell, P <: MaybePlayer, CELLS <: HList, TAIL <: HList](
                                                                                                     implicit playerAt: PlayerAt.Aux[Moves, C, P],
                                                                                                     tail: Aux[Moves, CELLS, TAIL]): Aux[Moves, C :: CELLS, P :: TAIL] = null
}

sealed trait LowPrioHas {
  implicit def ifTail[F[_], H, X <: HList](implicit ev: HasF[F, X]): HasF[F, H :: X] = null
}

object ListOps extends LowPrioHas {

  sealed trait HasF[F[_], X <: HList]

  implicit def ifhead[F[_], H, X <: HList](implicit ev: F[H]): HasF[F, H :: X] = null

  sealed trait ForallF[F[_], X <: HList]

  implicit def ifEmpty[F[_]]: ForallF[F, HNil] = null

  implicit def ifCons[F[_], H, X <: HList](implicit headEv: F[H], tailEv: ForallF[F, X]): ForallF[F, H :: X] = null
}

object Outcome {

  sealed trait Outcome

  class Winner[P <: Player] extends Outcome

  object InPlay extends Outcome

  object Draw extends Outcome

}

sealed trait LP {
  implicit def inPlay1[ST <: HList](implicit notBoth: HasF[IsNotP1, ST], hasEmpty: HasF[IsEmpty, ST]): RowOutcome.Aux[ST, InPlay.type] = null

}

object RowOutcome extends LP{

  sealed trait RowOutcome[ST <: HList] {
    type Out <: Outcome
  }

  type IsP1[T] = =:=[T, P1.type]
  type IsP2[T] = =:=[T, P1.type]
  type RowIsDraw[T, P] = =:=[T, Draw.type]
  type IsNotP1[T] = =:!=[T, P1.type]
  type IsNotP2[T] = =:!=[T, P2.type]
  type IsEmpty[T] = =:=[T, Empty.type]

  type Aux[ST <: HList, O <: Outcome] = RowOutcome[ST] {
    type Out = O
  }

  implicit def hasWinner1[ST <: HList](implicit winner: ForallF[IsP1, ST]): RowOutcome.Aux[ST, Winner[P1.type]] = null

  implicit def hasWinner2[ST <: HList](implicit winner: ForallF[IsP1, ST]): RowOutcome.Aux[ST, Winner[P2.type]] = null

  implicit def draw[ST <: HList](implicit p1: HasF[IsP1, ST], p2: HasF[IsP2, ST]): RowOutcome.Aux[ST, Draw.type] = null

  implicit def inPlay2[ST <: HList](implicit notBoth: HasF[IsNotP2, ST], hasEmpty: HasF[IsEmpty, ST]): RowOutcome.Aux[ST, InPlay.type] = null

  sealed trait MovesOutcome[Moves <: HList, CELLS <: HList, O <: Outcome]

  implicit def lift[Moves <: HList, CELLS <: HList, ST <: HList, O <: Outcome](implicit proj: Projection.Aux[Moves, CELLS, ST], rowResult: Aux[ST, O]): MovesOutcome[Moves, CELLS, O] = null
}

sealed trait LowPriority {
  implicit def wonT[MOVES <: HList, ROW1 <: HList, ROWS <: HList, P<:Player](implicit ev: Aux[MOVES, ROWS, Winner[P]]): Aux[MOVES, ROW1 :: ROWS, Winner[P]] = null
  implicit def InPlayDrawInPlay[MOVES<:HList, ROW1<: HList, ROWS <: HList](implicit ev1: MovesOutcome[MOVES, ROW1, InPlay.type], ev2 : Aux[MOVES, ROWS, Draw.type ]): Aux[MOVES, ROW1 :: ROWS, InPlay.type ] = null
  implicit def InPlayInPlayInPlay[MOVES<:HList, ROW1<: HList, ROWS <: HList](implicit ev1: MovesOutcome[MOVES, ROW1, InPlay.type], ev2 : Aux[MOVES, ROWS, InPlay.type ]): Aux[MOVES, ROW1 :: ROWS, InPlay.type ] = null
}

object GameOutcome extends LowPriority {

  sealed trait GameOutcome[Moves <: HList, ROWS <: HList] {
    type Out <: Outcome
  }

  type Aux[Moves <: HList, ROWS <: HList, O <: Outcome] = GameOutcome[Moves, ROWS] {
    type Out = O
  }

  implicit def drawOnZeroRows[MOVES<:HList]: Aux[MOVES, HNil, Draw.type ] = null
  implicit def drawCons[MOVES<:HList, ROW1<: HList, ROWS <: HList](implicit ev1: MovesOutcome[MOVES, ROW1, Draw.type], ev2 : Aux[MOVES, ROWS, Draw.type ]): Aux[MOVES, ROW1 :: ROWS, Draw.type ] = null
  implicit def wonH[MOVES <: HList, ROW1<: HList, ROWS <: HList, P<:Player](implicit ev: MovesOutcome[MOVES, ROW1, Winner[P]]): Aux[MOVES, ROW1 :: ROWS, Winner[P]] = null

  implicit def drawInPlayInPlay[MOVES<:HList, ROW1<: HList, ROWS <: HList](implicit  ev1: MovesOutcome[MOVES, ROW1, Draw.type], ev2 : Aux[MOVES, ROWS, InPlay.type ]): Aux[MOVES, ROW1 :: ROWS, InPlay.type ] = null

}

object TicTacToe {

  import Players._

  sealed trait Board {
    type Moves <: HList
  }

  type Aux[B <: Board, M] = B {
    type Moves = M
  }

  object EmptyBoard extends Board {
    type Moves = HNil
  }

  case class WithMove[B <: Board, C <: Cell](b: B, c: C) extends Board {
    type Moves = C :: b.Moves
  }

  def move[B <: Board, M <: HList, C <: Cell](board: Aux[B, M], cell: C)(implicit canPlay: GameOutcome.Aux[M, Cells.ALL_ROWS_TO_CHECK , InPlay.type ], isEmpty: PlayerAt.Aux[M, C, Empty.type]): WithMove[B, C] = {
    WithMove(board, cell)
  }
}


object Runner extends App {

  import Turn._
  import TicTacToe._
  import GameOutcome._
  import RowOutcome._
  import Projection._
  import Players._

  val emptyBoard = EmptyBoard
  val b1 = move(emptyBoard, NW)
  //val b2 = move(b1, NW) -- illegal, NW is already occupied, will not compile
  val b2 = move(b1, C)
  val b3 = move(b2, N)
  val b4 = move(b3, S)
  val b5 = move(b4, NE)
  //val b6 = move(b5, SW) -- illegal, the game is over, will not compile
}