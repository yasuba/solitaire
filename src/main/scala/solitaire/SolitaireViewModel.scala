package solitaire

import indigo.Point

case class SolitaireViewModel(
                               dragging: Option[DragState]
                             )

case class DragState(
                      cards: List[Card],
                      source: GameElement,
                      currentPosition: Point
                    )
