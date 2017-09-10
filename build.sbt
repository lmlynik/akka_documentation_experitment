scalaVersion in ThisBuild := "2.12.2"

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin(
    "org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in(Compile, console) := Seq() // macroparadise plugin doesn't work in repl yet.
)



val akkaV = "2.5.3"
val akkaHttpV = "10.0.9"
val scalaTestV = "3.0.1"

// Define macros in this project.
lazy val macros = project.settings(
  metaMacroSettings,
  // A dependency on scala.meta is required to write new-style macros, but not
  // to expand such macros.  This is similar to how it works for old-style
  // macros and a dependency on scala.reflect.
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
    "org.scalameta" %% "scalameta" % "1.8.0",
    "org.scalameta" %% "contrib" % "1.8.0",
    "org.scalameta" %% "testkit" % "1.8.0" % Test,
    "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )
)

// Use macros in this project.
lazy val app = project.settings(metaMacroSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % akkaHttpV
    )
  )
  .dependsOn(macros)