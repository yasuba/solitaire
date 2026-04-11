package solitaire

import indigo.*
import indigo.scenes.*
import indigo.shared.scenegraph.SceneUpdateFragment
import solitaire.HomeLayout.*

object HomeScene extends Scene[Unit, GameState, SolitaireViewModel] {
  val name: SceneName = SceneName("home")
  override type SceneModel = GameState
  override type SceneViewModel = SolitaireViewModel

  override def modelLens: Lens[GameState, GameState] = Lens.keepLatest[GameState]

  override def viewModelLens: Lens[SolitaireViewModel, SolitaireViewModel] = Lens.keepLatest[SolitaireViewModel]

  override def eventFilters: EventFilters = EventFilters.Permissive

  override def subSystems: Set[SubSystem] = Set.empty

  override def updateModel(context: SceneContext[Unit], state: GameState): GlobalEvent => Outcome[GameState] =
    case ViewportResize(vp) =>
      val newVp = vp.toSize
      Outcome(state.copy(viewport = newVp))

    case FrameTick =>
      if state.current.isWon then Outcome(state)
      else if !state.dealt then
        Outcome(state.copy(
          current = SolitaireModel.deal(context.dice),
          dealt = true
        ))
      else if state.current.canAutoComplete then state.current.autoCompleteStep.map(s => Outcome(state.copy(current = s, timer = state.timer + context.delta.toDouble))).getOrElse(Outcome(state))
      else if state.started then Outcome(state.copy(timer = state.timer + context.delta.toDouble))
      else Outcome(state)

    case e: PointerEvent.PointerUp =>
      hitTest(e.position.x, e.position.y, state.current) match
        case Some(GameElement.Stock) => Outcome(state.onStockTapped)
        case Some(GameElement.Waste) =>
          state.current.moveWasteToFoundation.map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state))
        case Some(GameElement.Foundation(_)) => Outcome(state)
        case Some(GameElement.TableauCard(col, cardIndex)) =>
          if cardIndex == 0 then state.current.moveTableauToFoundation(col).map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state)) else Outcome(state)
        case Some(GameElement.UndoButton) => Outcome(state.undo)
        case None => Outcome(state)

    case m: SolitaireEvent.MoveCards =>
      (m.source, m.target) match {
        case (GameElement.Waste, GameElement.Foundation) => state.current.moveWasteToFoundation.map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state))
        case (GameElement.Waste, GameElement.TableauCard(col, _)) => state.current.moveWasteToTableau(col).map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state))
        case (GameElement.TableauCard(col, cardIndex), GameElement.Foundation) =>
          if cardIndex == 0 then state.current.moveTableauToFoundation(col).map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state)) else Outcome(state)
        case (GameElement.TableauCard(sourceCol, sourceCardIndex), GameElement.TableauCard(targetCol, _)) =>
          state.current.moveTableauToTableau(sourceCol, sourceCardIndex, targetCol).map(s => Outcome(state.applyMove(s))).getOrElse(Outcome(state))
        case (_, _) => Outcome(state)
      }

  override def updateViewModel(context: SceneContext[Unit], state: GameState, viewModel: SolitaireViewModel): GlobalEvent => Outcome[SolitaireViewModel] =
    case e: PointerEvent.PointerDown =>
      hitTest(e.position.x, e.position.y, state.current) match
        case Some(GameElement.Stock) => Outcome(viewModel)
        case Some(GameElement.Waste) =>
          state.current.waste match {
            case Nil => Outcome(viewModel)
            case head :: _ => Outcome(viewModel.copy(dragging = Some(DragState(List(head), GameElement.Waste, e.position))))
          }
        case Some(GameElement.Foundation(_)) => Outcome(viewModel)
        case Some(GameElement.TableauCard(col, cardIndex)) =>
          val tappedCard = state.current.tableau(col)(cardIndex)
          val selectedCards = state.current.tableau(col).drop(cardIndex)
          if tappedCard.faceUp then
            Outcome(viewModel.copy(dragging = Some(DragState(selectedCards, GameElement.TableauCard(col, cardIndex), e.position))))
          else Outcome(viewModel)
        case _ => Outcome(viewModel)
    case e: PointerEvent.PointerMove => viewModel.dragging match
      case None => Outcome(viewModel)
      case Some(drag) =>
        Outcome(viewModel.copy(dragging = Some(drag.copy(currentPosition = e.position))))
    case e: PointerEvent.PointerUp => viewModel.dragging match
      case None => Outcome(viewModel)
      case Some(drag) =>
        hitTest(e.position.x, e.position.y, state.current) match
          case Some(target) =>
            Outcome(viewModel.copy(dragging = None))
              .addGlobalEvents(SolitaireEvent.MoveCards(drag.source, target))
          case None =>
            Outcome(viewModel.copy(dragging = None))
    case _ => Outcome(viewModel)

  override def present(context: SceneContext[Unit], model: GameState, viewModel: SolitaireViewModel): Outcome[SceneUpdateFragment] =
    val vp = model.viewport
    val bgNode: SceneNode =
      Shape.Box(Rectangle(0, 0, bw(vp), bh(vp)), Fill.Color(RGBA(0.851, 0.922, 0.957, 1.0)))

    val nodes = List(bgNode)
    Outcome(SceneUpdateFragment(Layer(nodes *)))
}

object HomeLayout {
  def bh(viewport: Size): Int = viewport.height

  def bw(viewport: Size): Int = viewport.width

  val cardWidth = 80
  val cardHeight = 120 // exactly 2:3 ratio, close enough to standard
  val padding = 10

  // Top row
  val topRowY = 20
  val stockX = 20
  val wasteX = 110 // stockX + cardWidth + padding

  // Foundations start further right
  val foundationStartX = 380 // leaves gap in middle

  // Tableau
  val tableauY = 160 // below top row with some gap
  val tableauStartX = 20

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
      column.zipWithIndex.findLast { (card, cardIndex) =>
        val cardY = tableauY + column.take(cardIndex).map(c => if c.faceUp then 30 else 20).sum
        ty >= cardY && ty <= cardY + cardHeight
      }.map { (card, cardIndex) =>
        (columnIndex, cardIndex)
      }
    }
  }

  def hitTest(tx: Double, ty: Double, state: SolitaireModel): Option[GameElement] =
    Option.when(tappedStock(tx, ty))(GameElement.Stock)
      .orElse(Option.when(tappedWaste(tx, ty))(GameElement.Waste))
      .orElse((0 to 3).collectFirst { case i if tappedFoundation(i, tx, ty) => GameElement.Foundation(i) })
      .orElse(tappedTableau(tx, ty, state.tableau).map(GameElement.TableauCard(_, _)))
}

enum GameElement:
  case Stock
  case Waste
  case Foundation(index: Int)
  case TableauCard(columnIndex: Int, cardIndex: Int)
  case UndoButton

enum SolitaireEvent extends GlobalEvent:
  case MoveCards(source: GameElement, target: GameElement)
