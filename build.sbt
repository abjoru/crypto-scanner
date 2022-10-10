ThisBuild / organization := "com.bjoru"
ThisBuild / version      := "0.1"
ThisBuild / scalaVersion := "3.2.0"

//////////////
// Versions //
//////////////

lazy val catsVersion       = "2.8.0"
lazy val catsEffectVersion = "3.3.12"
lazy val http4sVersion     = "0.23.16"
lazy val circeVersion      = "0.14.1"
lazy val pureConfigVersion = "0.17.1"

lazy val munitVersion = "0.7.29"

/////////////
// Modules //
/////////////

lazy val ethereumApi = (project in file("chains/ethereum"))
  .dependsOn(core)
  .settings(
    name := "crypto-scanner-ethereum",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-circe"        % http4sVersion,
      "org.http4s" %% "http4s-dsl"          % http4sVersion
    )
  )

//////////////
// Standard //
//////////////

lazy val core = (project in file("core"))
  .settings(
    name := "crypto-scanner-core",
    libraryDependencies ++= Seq(
      "org.typelevel"         %% "cats-core"       % catsVersion,
      "org.typelevel"         %% "cats-effect"     % catsEffectVersion,
      "io.circe"              %% "circe-core"      % circeVersion,
      "io.circe"              %% "circe-generic"   % circeVersion,
      "org.http4s"            %% "http4s-core"     % http4sVersion,
      "com.github.pureconfig" %% "pureconfig-core" % pureConfigVersion
    )
  )

lazy val testKit = (project in file("testKit"))
  .settings(
    name := "crypto-scanner-testkit",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"            % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitVersion % Test
    )
  )
