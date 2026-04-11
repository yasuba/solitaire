package solitaire

import indigo.Dice
import solitaire.Rank.*
import solitaire.SolitaireModel.flipTopCard

import scala.annotation.tailrec

enum Suit {
  case Hearts, Diamonds, Clubs, Spades
    def colour: Colour = this match {
      case s if s == Hearts || s == Diamonds => Colour.Red
      case _ => Colour.Black
    }
  }

enum Colour:
  case Red, Black

enum Rank:
  case Ace, Two, Three, Four, Five, Six, Seven,
  Eight, Nine, Ten, Jack, Queen, King

case class Card(suit: Suit, rank: Rank, faceUp: Boolean)

case class SolitaireModel(
                           stock: List[Card],        // the draw pile (face down)
                           waste: List[Card],        // cards turned from stock (face up)
                           foundations: Vector[List[Card]], // 4 piles, one per suit, Ace→King
                           tableau: Vector[List[Card]]      // 7 columns, the main play area
                         ) {
  def canMoveToFoundation(card: Card, foundation: List[Card]): Boolean = {
    def oneAbove(ordinal: Int) = ordinal + 1

    foundation.isEmpty && card.rank == Ace ||
      foundation.headOption.exists(c => c.suit == card.suit && card.rank.ordinal == oneAbove(c.rank.ordinal))
  }

  def moveToFoundation(card: Card): Option[SolitaireModel] =
    foundations.zipWithIndex
      .find((f, _) => canMoveToFoundation(card, f))
      .map((f, i) => copy(
        foundations = foundations.updated(i, card :: f)
      ))

  def moveWasteToFoundation: Option[SolitaireModel] =
    waste.headOption.flatMap { c =>
      val maybeMovedToFoundation = moveToFoundation(c)
      if maybeMovedToFoundation.nonEmpty then maybeMovedToFoundation.map(s => s.copy(waste = s.waste.tail))
      else None
    }

  def moveTableauToFoundation(columnIndex: Int): Option[SolitaireModel] = {
    val column = tableau(columnIndex)
    column.lastOption match {
      case Some(card) =>
        moveToFoundation(card)
          .map(s => s.copy(tableau = s.tableau.updated(columnIndex, flipTopCard(column.init))))
      case None => None
    }
  }

  def isOppositeColour(card: Card, card2: Card): Boolean =
    card.suit.colour != card2.suit.colour

  def isOneBelow(card: Card, card2: Card): Boolean =
    card.rank.ordinal - 1 == card2.rank.ordinal

  def canMoveToTableau(card: Card, column: List[Card]): Boolean =
    column.lastOption match {
      case None => card.rank == King
      case Some(topCard) => isOppositeColour(topCard, card) && isOneBelow(topCard, card)
    }

  def moveTableauToTableau(fromIndex: Int, cardIndex: Int, toIndex: Int): Option[SolitaireModel] = {
    val (remaining, sequence) = tableau(fromIndex).splitAt(cardIndex)
    sequence.headOption.filter(card => canMoveToTableau(card, tableau(toIndex))).map(card => copy(tableau =
      tableau
        .updated(fromIndex, flipTopCard(remaining))
        .updated(toIndex, tableau(toIndex) ::: sequence)
    ))
  }

  def moveWasteToTableau(toIndex: Int): Option[SolitaireModel] =
    waste.headOption
      .filter(card => canMoveToTableau(card, tableau(toIndex)))
      .map(card => copy(
        waste = waste.tail,
        tableau = tableau.updated(toIndex, tableau(toIndex) :+ card)
      ))

  def autoCompleteStep: Option[SolitaireModel] =
    (moveWasteToFoundation :: (0 to 6).toList.map(col => moveTableauToFoundation(col))).collectFirst { case Some(s) => s }
    
  def canAutoComplete: Boolean =
    stock.isEmpty && tableau.flatten.forall(_.faceUp)

  def isWon: Boolean =
    foundations.flatten.size == 52
}

object SolitaireModel {
  val initial: SolitaireModel = SolitaireModel(
    List(),
    List(),
    Vector.fill(4)(Nil),
    Vector.empty
  )

  def flipTopCard(column: List[Card]): List[Card] =
    column match
      case Nil => Nil
      case _   => column.init :+ column.last.copy(faceUp = true)

  def fullDeck: List[Card] =
    Suit.values.flatMap { s =>
      Rank.values.map { r =>
        Card(s, r, faceUp = false)
      }
    }.toList

  def shuffle(deck: List[Card], dice: Dice): List[Card] = {
    @tailrec
    def loop(acc: List[Card], rest: List[Card]): List[Card] =
      if (rest.nonEmpty) {
        val index = dice.rollFromZero(rest.size)
        val randomCard = rest(index)
        loop(randomCard :: acc, rest.patch(index, Nil, 1))
      } else acc

    loop(Nil, deck)
  }
  
  def deal(dice: Dice): SolitaireModel = {
    val (tableau, remaining) = (1 to 7).foldLeft((Vector.empty[List[Card]], shuffle(fullDeck, dice))) {
      case ((acc, remaining), n) =>
        val (column, rest) = remaining.splitAt(n)
        val flippedColumn = column.init ::: column.lastOption.map(_.copy(faceUp = true)).toList
        (acc :+ flippedColumn, rest)
    }
    initial.copy(tableau = tableau, stock = remaining)
  }
}