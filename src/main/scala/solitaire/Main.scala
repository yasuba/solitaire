package solitaire

import indigo.*
import indigo.shared.config.ResizePolicy
import indigo.scenes.{Scene, SceneName}

object Main extends IndigoGame[Unit, Unit, GameState, SolitaireViewModel] {

  def main(args: Array[String]): Unit =
    launch("indigo-container")

  override def scenes(bootData: Unit): NonEmptyList[Scene[Unit, GameState, SolitaireViewModel]] = NonEmptyList(HomeScene)

  override def initialScene(bootData: Unit): Option[SceneName] = Some(HomeScene.name)

  override def eventFilters: EventFilters = EventFilters.Permissive

  private val suits = List("heart", "diamond", "club", "spade")
  private val ranks = List("Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King")

  private val cardAssets: Set[AssetType] =
    (for {
      suit <- suits
      rank <- ranks
    } yield AssetType.Image(AssetName(s"$suit$rank"), AssetPath(s"assets/cards/$suit$rank.png"))
    ).toSet +
      AssetType.Image(AssetName("blueBack"), AssetPath("assets/cards/blueBack.png"))


  override def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(
      BootResult.configOnly(
        GameConfig.default
          .withClearColor(RGBA(0.980, 0.957, 0.925, 1.0))
          .withResizePolicy(ResizePolicy.Resize)
      )
        .withAssets(
          cardAssets)
    )

  override def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  override def initialModel(startupData: Unit): Outcome[GameState] = 
    Outcome(GameState.initial)


  override def initialViewModel(startupData: Unit, model: GameState): Outcome[SolitaireViewModel] = Outcome((SolitaireViewModel(dragging = None)))

  override def updateModel(context: FrameContext[Unit], model: GameState): GlobalEvent => Outcome[GameState] = _ => Outcome(model)

  override def updateViewModel(context: FrameContext[Unit], model: GameState, viewModel: SolitaireViewModel): GlobalEvent => Outcome[SolitaireViewModel] = _ => Outcome(viewModel)

  override def present(context: FrameContext[Unit], model: GameState, viewModel: SolitaireViewModel): Outcome[SceneUpdateFragment] = Outcome(SceneUpdateFragment.empty)
}
