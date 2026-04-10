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
                            started: Boolean
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
      false
    )