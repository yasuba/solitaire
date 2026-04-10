
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "solitaire",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,

    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("solitaire.Main"),

    libraryDependencies ++= Seq(
      "io.indigoengine" %%% "indigo" % "0.16.0"
    )
  )
