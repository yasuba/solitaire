package solitaire

import indigo.*
import indigo.scenes.*
import indigo.shared.scenegraph.SceneUpdateFragment
import solitaire.HomeLayout.*

object HomeScene extends Scene[Unit, GameState, Unit] {
  val name: SceneName = SceneName("home")
  override type SceneModel = GameState
  override type SceneViewModel = Unit

  override def modelLens: Lens[GameState, GameState] = Lens.keepLatest[GameState]

  override def viewModelLens: Lens[Unit, Unit] = Lens.unit

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


    case e: PointerEvent.PointerDown =>
      val vp = state.viewport
      val tapX = e.position.x
      val tapY = e.position.y
      Outcome(state)


  override def updateViewModel(context: SceneContext[Unit], model: GameState, viewModel: Unit): GlobalEvent => Outcome[Unit] = _ => Outcome(())

  override def present(context: SceneContext[Unit], model: GameState, viewModel: Unit): Outcome[SceneUpdateFragment] =
    val vp = model.viewport
    val bgNode: SceneNode =
      Shape.Box(Rectangle(0, 0, bw(vp), bh(vp)), Fill.Color(RGBA(0.851, 0.922, 0.957, 1.0)))

    val nodes = List(bgNode)
    Outcome(SceneUpdateFragment(Layer(nodes *)))

    def withinCard(x: Double, y: Double, cardX: Double, cardY: Double): Boolean =
      x >= cardX && x <= cardX + HomeLayout.cardWidth &&
        y >= cardY && y <= cardY + HomeLayout.cardHeight
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
}

enum GameElement:
  case Stock
  case Waste
  case Foundation(index: Int)
  case TableauCard(columnIndex: Int, cardIndex: Int)
