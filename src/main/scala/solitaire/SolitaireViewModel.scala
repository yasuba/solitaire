package solitaire

import indigo.Point

case class SolitaireViewModel(
                               dragging: Option[DragState],
                               isDragging: Boolean = false
                             )

case class DragState(
                      cards: List[Card],
                      source: GameElement,
                      currentPosition: Point
                    )
