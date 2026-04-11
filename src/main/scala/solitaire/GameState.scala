package solitaire

import indigo.shared.datatypes.Size
import solitaire.Rank.{Ace, King}

final case class GameState(
                            current: SolitaireModel,
                            history: List[SolitaireModel],
                            timer: Double,
                            personalRecord: Option[Double],
                            dealt: Boolean,
                            viewport: Size,
                            started: Boolean,
                            snapBack: Option[(GameElement, List[Card])],
                            autoCompleteTimer: Double
                          ) {
  def drawFromStock: GameState =
    copy(
      current = current.copy(
        stock = current.stock.drop(3),
        waste = current.stock.take(3).map(_.copy(faceUp = true)).reverse ::: current.waste
      )
    )

  def recycleWaste: GameState =
    copy(
      current = current.copy(
        stock = current.waste.map(_.copy(faceUp = false)),
        waste = Nil
      )
    )

  def onStockTapped: GameState = {
    if (current.stock.isEmpty) recycleWaste.copy(history = this.current :: history)
    else drawFromStock.copy(history = this.current :: history)
  }

  def applyMove(newModel: SolitaireModel): GameState =
    val prePickupModel = snapBack match
      case Some((source, cards)) => restoreCards(source, cards).current
      case None => current
    copy(
      current = newModel,
      history = prePickupModel :: history,
      started = true,
      snapBack = None
    )

  def undo: GameState =
    history match
      case Nil => this
      case previous :: rest =>
        copy(
          current = previous,
          history = rest
        )

  def restoreCards(source: GameElement, cards: List[Card]): GameState =
    source match
      case GameElement.Waste =>
        copy(current = current.copy(waste = cards ::: current.waste))
      case GameElement.TableauCard(col, _) =>
        copy(current = current.copy(tableau = current.tableau.updated(col, cards)))
      case _ => this

  def failedMove: GameState =
    snapBack match
      case None => this
      case Some((source, cards)) => restoreCards(source, cards).copy(snapBack = None)    
}

object GameState:

  val initial: GameState =
    GameState(
      SolitaireModel.initial,
      List.empty,
      0.0,
      None,
      false,
      Size(800, 600),
      false,
      None,
      0.0
    )
