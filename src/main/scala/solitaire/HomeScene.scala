package solitaire

import indigo.*
import indigo.scenes.*
import indigo.shared.scenegraph.SceneUpdateFragment
import solitaire.HomeLayout.*
import solitaire.SolitaireModel.flipTopCard

object HomeScene extends Scene[Unit, GameState, SolitaireViewModel] {
  val name: SceneName = SceneName("home")
  override type SceneModel = GameState
  override type SceneViewModel = SolitaireViewModel

  override def modelLens: Lens[GameState, GameState] = Lens.keepLatest[GameState]

  override def viewModelLens: Lens[SolitaireViewModel, SolitaireViewModel] = Lens.keepLatest[SolitaireViewModel]

  override def eventFilters: EventFilters = EventFilters.Permissive

  override def subSystems: Set[SubSystem] = Set.empty

  override def updateModel(context: SceneContext[Unit], state: GameState): GlobalEvent => Outcome[GameState] = event =>
    val layout = HomeLayout(state.viewport)
    import layout.*
    event match {
      case ViewportResize(vp) =>
        IndigoLogger.info(s"Viewport: ${vp.width} x ${vp.height}")
        val newVp = vp.toSize
        Outcome(state.copy(viewport = newVp))

      case FrameTick =>
        IndigoLogger.info(s"FrameTick: dealt=${state.dealt} isWon=${state.current.isWon}")
        if state.current.isWon then Outcome(state)
        else if !state.dealt then
          Outcome(state.copy(
            current = SolitaireModel.deal(context.dice),
            dealt = true
          ))
        else if state.current.canAutoComplete then {
          if state.autoCompleteTimer >= 0.5 then
            state.current.autoCompleteStep.map(s => Outcome(state.copy(current = s, timer = state.timer + context.delta.toDouble)))
              .getOrElse(Outcome(state))
          else
            Outcome(state.copy(
              autoCompleteTimer = state.autoCompleteTimer + context.delta.toDouble,
              timer = state.timer + context.delta.toDouble
            ))
        } else if state.started then Outcome(state.copy(timer = state.timer + context.delta.toDouble))
        else Outcome(state)

      case e: PointerEvent.PointerUp =>
        if state.snapBack.isDefined then Outcome(state) // drag in progress — MoveCards handles it
        else hitTest(e.position.x, e.position.y, state.current, state.viewport) match
          case Some(GameElement.Stock) => Outcome(state.onStockTapped)
          case Some(GameElement.Waste) =>
            state.current.moveWasteToFoundation.map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state))
          case Some(GameElement.Foundation(_)) => Outcome(state)
          case Some(GameElement.TableauCard(col, cardIndex)) =>
            if cardIndex == state.current.tableau(col).size - 1 then state.current.moveTableauToFoundation(col).map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state)) else Outcome(state)
          case Some(GameElement.UndoButton) => Outcome(state.undo)
          case Some(GameElement.DealAgainButton) => Outcome(GameState.initial)
          case None => Outcome(state)

      case p: SolitaireEvent.PickupCards =>
        p.source match
          case GameElement.Waste =>
            val cards = state.current.waste.take(1)
            Outcome(state.copy(
              current = state.current.copy(waste = state.current.waste.tail),
              snapBack = Some((GameElement.Waste, cards))
            ))
          case GameElement.TableauCard(col, cardIndex) =>
            val originalColumn = state.current.tableau(col)
            val remaining = originalColumn.take(cardIndex)
            Outcome(state.copy(
              current = state.current.copy(
                tableau = state.current.tableau.updated(col, flipTopCard(remaining))
              ),
              snapBack = Some((p.source, originalColumn))
            ))
          case _ => Outcome(state)

      case m: SolitaireEvent.MoveCards =>
        IndigoLogger.info(s"MoveCards: ${m.source} -> ${m.target}")
        if m.source == m.target then Outcome(state.failedMove)
        else
          val pickedCards = state.snapBack match
            case Some((_, originalCards)) => m.source match
              case GameElement.TableauCard(_, cardIndex) => originalCards.drop(cardIndex)
              case _ => originalCards
            case None => Nil
          if pickedCards.isEmpty then Outcome(state.failedMove)
          else m.target match
            case GameElement.Foundation(_) =>
              pickedCards.headOption
                .flatMap(card => state.current.moveToFoundation(card))
                .map(s => Outcome(state.applyMove(s)))
                .getOrElse(Outcome(state.failedMove))
            case GameElement.TableauCard(targetCol, _) =>
              pickedCards.headOption
                .filter(card => state.current.canMoveToTableau(card, state.current.tableau(targetCol)))
                .map(_ => state.current.copy(
                  tableau = state.current.tableau.updated(targetCol, state.current.tableau(targetCol) ::: pickedCards)
                ))
                .map(newModel => Outcome(state.applyMove(newModel)))
                .getOrElse(Outcome(state.failedMove))
            case _ => Outcome(state.failedMove)

      case _ => Outcome(state)
    }

  override def updateViewModel(context: SceneContext[Unit], state: GameState, viewModel: SolitaireViewModel): GlobalEvent => Outcome[SolitaireViewModel] = event =>
    val layout = HomeLayout(state.viewport)
    import layout.*
    event match {
      case e: PointerEvent.PointerDown =>
        hitTest(e.position.x, e.position.y, state.current, state.viewport) match
          case Some(GameElement.Stock) => Outcome(viewModel)
          case Some(GameElement.Waste) =>
            state.current.waste match {
              case Nil => Outcome(viewModel)
              case head :: _ =>
                Outcome(viewModel.copy(
                  dragging = Some(DragState(List(head), GameElement.Waste, e.position)),
                  isDragging = false
                ))
            }
          case Some(GameElement.Foundation(_)) => Outcome(viewModel)
          case Some(GameElement.TableauCard(col, cardIndex)) =>
            val tappedCard = state.current.tableau(col)(cardIndex)
            val selectedCards = state.current.tableau(col).drop(cardIndex)
            if tappedCard.faceUp then
              Outcome(viewModel.copy(
                dragging = Some(DragState(selectedCards, GameElement.TableauCard(col, cardIndex), e.position)),
                isDragging = false
              ))
            else Outcome(viewModel)
          case _ => Outcome(viewModel)

      case e: PointerEvent.PointerMove => viewModel.dragging match
        case None => Outcome(viewModel)
        case Some(drag) =>
          if !viewModel.isDragging then
            Outcome(viewModel.copy(dragging = Some(drag.copy(currentPosition = e.position)), isDragging = true))
              .addGlobalEvents(SolitaireEvent.PickupCards(drag.source))
          else
            Outcome(viewModel.copy(dragging = Some(drag.copy(currentPosition = e.position))))

      case e: PointerEvent.PointerUp =>
        viewModel.dragging match
          case None => Outcome(viewModel)
          case Some(drag) =>
            if !viewModel.isDragging then
              Outcome(viewModel.copy(dragging = None, isDragging = false))
            else
              hitTest(e.position.x, e.position.y, state.current, state.viewport) match
                case Some(target) =>
                  Outcome(viewModel.copy(dragging = None, isDragging = false))
                    .addGlobalEvents(SolitaireEvent.MoveCards(drag.source, target))
                case None =>
                  Outcome(viewModel.copy(dragging = None, isDragging = false))
                    .addGlobalEvents(SolitaireEvent.MoveCards(drag.source, drag.source))

      case _ => Outcome(viewModel)
    }

  override def present(context: SceneContext[Unit], model: GameState, viewModel: SolitaireViewModel): Outcome[SceneUpdateFragment] =
    val layout = HomeLayout(model.viewport)
    import layout.*

    def rankToString(rank: Rank): String = rank match {
      case Rank.Ace => "A"
      case Rank.Two => "2"
      case Rank.Three => "3"
      case Rank.Four => "4"
      case Rank.Five => "5"
      case Rank.Six => "6"
      case Rank.Seven => "7"
      case Rank.Eight => "8"
      case Rank.Nine => "9"
      case Rank.Ten => "10"
      case Rank.Jack => "J"
      case Rank.Queen => "Q"
      case Rank.King => "K"
    }

    def suitToString(suit: Suit): String = suit match {
      case Suit.Clubs => "♣"
      case Suit.Spades => "♠"
      case Suit.Hearts => "♥"
      case Suit.Diamonds => "♦"
    }

    def renderCard(card: Card, x: Int, y: Int): Batch[SceneNode] =
      if !card.faceUp then
        Batch(
          Shape.Box(Rectangle(x, y, cardWidth, cardHeight), Fill.Color(RGBA.Blue),
            Stroke(2, RGBA.Black))
        )
      else
        val colour = if card.suit.colour == Colour.Red then RGBA.Red else RGBA.Black
        val label = rankToString(card.rank) + suitToString(card.suit)
        Batch(
          Shape.Box(Rectangle(x, y, cardWidth, cardHeight), Fill.Color(RGBA.White), Stroke(2, RGBA.Black)),
          TextBox(label)
            .withFontSize(Pixels(fontSize))
            .withColor(colour)
            .moveTo(Point(x + 2, y + 2))
            .withSize(Size(cardWidth, fontSize + 4))
        )

    def renderStock(state: SolitaireModel): Batch[SceneNode] =
      state.stock.headOption match {
        case None => Batch(Shape.Box(Rectangle(stockX, topRowY, cardWidth, cardHeight), Fill.None, Stroke(2, RGBA.Black)))
        case Some(c) => renderCard(c, stockX, topRowY)
      }

    def renderWaste(state: SolitaireModel): Batch[SceneNode] =
      Batch.fromList(state.waste.take(3).reverse.zipWithIndex).flatMap { (card, i) =>
        renderCard(card, wasteX + i * (cardWidth / 4), topRowY)
      }

    def renderFoundations(state: SolitaireModel): Batch[SceneNode] =
      Batch.fromList(state.foundations.zipWithIndex.toList).flatMap { (col, i) =>
        val x = foundationStartX + i * (cardWidth + padding)
        col.headOption match
          case Some(card) => renderCard(card, x, topRowY)
          case None => Batch(Shape.Box(Rectangle(x, topRowY, cardWidth, cardHeight), Fill.None, Stroke(2, RGBA.White)))
      }

    def renderTableau(state: SolitaireModel): Batch[SceneNode] =
      Batch.fromList(state.tableau.zipWithIndex.toList).flatMap { (column, colIndex) =>
        val x = tableauStartX + colIndex * (cardWidth + padding)
        Batch.fromList(column.zipWithIndex).flatMap { (card, cardIndex) =>
          val y = tableauY + column.take(cardIndex).map(c => if c.faceUp then faceUpOffset else faceDownOffset).sum
          renderCard(card, x, y)
        }
      }

    def renderDragging(viewModel: SolitaireViewModel): Batch[SceneNode] =
      viewModel.dragging match
        case None => Batch.empty
        case Some(drag) => Batch.fromList(drag.cards.zipWithIndex).flatMap { (card, i) =>
          val x = drag.currentPosition.x
          val y = drag.currentPosition.y + drag.cards.take(i).map(c => if c.faceUp then faceUpOffset else faceDownOffset).sum
          renderCard(card, x, y)
        }

    def formatTimer(timer: Double): String =
      val minutes = (timer / 60).toInt
      val seconds = (timer % 60).toInt
      f"$minutes%02d:$seconds%02d"

    def renderTimer(model: GameState): Batch[SceneNode] =
      val display = formatTimer(model.timer)
      Batch(
        TextBox(display)
          .withFontSize(Pixels(12))
          .withColor(RGBA.White)
          .alignCenter
          .withSize(Size(100, 20))
          .moveTo(Point(model.viewport.width / 2 - 50, 5))
      )

    def renderUndoButton: Batch[SceneNode] = {
      val x = model.viewport.width - 40 - padding
      val y = model.viewport.height - 40 - padding
      Batch(
        Shape.Box(Rectangle(x, y, 40, 40), Fill.Color(RGBA.White), Stroke(2, RGBA.Red)),
        TextBox("Undo")
          .withFontSize(Pixels(14))
          .withColor(RGBA.Red)
          .alignCenter
          .withSize(Size(40, 40))
          .moveTo(Point(x, y + 10))
      )
    }

    def renderDealAgainButton: Batch[SceneNode] = {
      val x = model.viewport.width - 80 - padding*2
      val y = model.viewport.height - 40 - padding
      Batch(
        Shape.Box(Rectangle(x, y, 40, 40), Fill.Color(RGBA.White), Stroke(2, RGBA.Red)),
        TextBox("Deal")
          .withFontSize(Pixels(14))
          .withColor(RGBA.Red)
          .alignCenter
          .withSize(Size(40, 40))
          .moveTo(Point(x, y + 10))
      )
    }

    def renderWinScreen(model: GameState): Batch[SceneNode] =
      if !model.current.isWon then Batch.empty
      else
        Batch(
          // semi-transparent dark overlay
          Shape.Box(Rectangle(0, 0, model.viewport.width, model.viewport.height), Fill.Color(RGBA(0, 0, 0, 0.7))),
          // congratulations text
          TextBox("You Win! 🎉")
            .withFontSize(Pixels(40))
            .withColor(RGBA.White)
            .alignCenter
            .withSize(Size(model.viewport.width, 60))
            .moveTo(Point(0, model.viewport.height / 2 - 60)),
          // time taken
          TextBox(s"Time: ${formatTimer(model.timer)}")
            .withFontSize(Pixels(24))
            .withColor(RGBA.White)
            .alignCenter
            .withSize(Size(model.viewport.width, 40))
            .moveTo(Point(0, model.viewport.height / 2))
        )

    val bgNode: SceneNode =
      Shape.Box(Rectangle(0, 0, bw, bh), Fill.Color(RGBA(0.110, 0.529, 0.035, 1.0))) // #1c8709

    val nodes = Batch(bgNode) ++ renderTimer(model) ++ renderStock(model.current)
      ++ renderWaste(model.current)
      ++ renderFoundations(model.current)
      ++ renderTableau(model.current)
      ++ renderDragging(viewModel)
      ++ renderUndoButton
      ++ renderDealAgainButton
      ++ renderWinScreen(model)

    Outcome(SceneUpdateFragment(nodes))
}

case class HomeLayout(vp: Size) {

  def bh: Int = vp.height

  def bw: Int = vp.width

  val margin = 5

  def cardWidth: Int = (vp.width - 2 * margin) / 8

  def cardHeight: Int = (cardWidth * 1.4).toInt

  def padding: Int = (vp.width - 7 * cardWidth) / 8

  def topRowY: Int = padding

  def stockX: Int = padding

  def wasteX: Int = stockX + cardWidth + padding

  def foundationStartX: Int = vp.width - 4 * (cardWidth + padding)

  def tableauStartX: Int = padding

  def tableauY: Int = topRowY + cardHeight + padding

  def faceUpOffset: Int = (cardHeight * 0.25).toInt.max(12)
  def faceDownOffset: Int = (cardHeight * 0.15).toInt.max(8)
  def fontSize: Int = (cardWidth * 0.3).toInt.max(10)

  def withinCard(x: Double, y: Double, cardX: Double, cardY: Double): Boolean =
    x >= cardX && x <= cardX + cardWidth &&
      y >= cardY && y <= cardY + cardHeight

  def tappedStock(tx: Double, ty: Double): Boolean =
    withinCard(tx, ty, stockX, topRowY)

  def tappedWaste(tx: Double, ty: Double): Boolean =
    withinCard(tx, ty, wasteX, topRowY)

  def tappedFoundation(index: Int, tx: Double, ty: Double): Boolean =
    val foundationX = foundationStartX + index * (cardWidth + padding)
    withinCard(tx, ty, foundationX, topRowY)

  def tappedTableau(tx: Double, ty: Double, tableau: Vector[List[Card]]): Option[(Int, Int)] = {
    val columnIndex = ((tx - tableauStartX) / (cardWidth + padding)).toInt
    if columnIndex < 0 || columnIndex > 6 then None
    else {
      val column = tableau(columnIndex)
      if column.isEmpty then
        if ty >= tableauY && ty <= tableauY + cardHeight then Some((columnIndex, 0))
        else None
      else
        column.zipWithIndex.findLast { (card, cardIndex) =>
          val cardY = tableauY + column.take(cardIndex).map(c => if c.faceUp then faceUpOffset else faceDownOffset).sum
          ty >= cardY && ty <= cardY + cardHeight
        }.map { (card, cardIndex) =>
          (columnIndex, cardIndex)
        }
    }
  }

  def tappedUndoButton(tx: Double, ty: Double, viewport: Size): Boolean = {
    val buttonX = viewport.width - 40 - padding
    val buttonY = viewport.height - 40 - padding
    tx >= buttonX && tx <= buttonX + 40 &&
      ty >= buttonY && ty <= buttonY + 40
  }

  def tappedDealAgainButton(tx: Double, ty: Double, viewport: Size): Boolean = {
    val buttonX = viewport.width - 80 - padding*2
    val buttonY = viewport.height - 40 - padding
    tx >= buttonX && tx <= buttonX + 40 &&
      ty >= buttonY && ty <= buttonY + 40
  }

  def hitTest(tx: Double, ty: Double, state: SolitaireModel, viewport: Size): Option[GameElement] =
    Option.when(tappedUndoButton(tx, ty, viewport))(GameElement.UndoButton)
      .orElse(Option.when(tappedDealAgainButton(tx, ty, viewport))(GameElement.DealAgainButton))
      .orElse(Option.when(tappedStock(tx, ty))(GameElement.Stock)
        .orElse(Option.when(tappedWaste(tx, ty))(GameElement.Waste))
        .orElse((0 to 3).collectFirst { case i if tappedFoundation(i, tx, ty) => GameElement.Foundation(i) })
        .orElse(tappedTableau(tx, ty, state.tableau).map(GameElement.TableauCard(_, _))))

}

enum GameElement:
  case Stock
  case Waste
  case Foundation(index: Int)
  case TableauCard(columnIndex: Int, cardIndex: Int)
  case UndoButton
  case DealAgainButton

enum SolitaireEvent extends GlobalEvent:
  case MoveCards(source: GameElement, target: GameElement)
  case PickupCards(source: GameElement)
