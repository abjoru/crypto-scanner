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
lazy val snakeyamlVersion  = "1.32"
lazy val stringdistVersion = "1.2.7"

lazy val munitVersion = "0.7.29"

/////////////
// Modules //
/////////////

lazy val core = (project in file("core"))
  .settings(
    name := "cryptosis-core",
    libraryDependencies ++= Seq(
      "org.typelevel"         %% "cats-core"           % catsVersion,
      "org.typelevel"         %% "cats-effect"         % catsEffectVersion,
      "io.circe"              %% "circe-core"          % circeVersion,
      "io.circe"              %% "circe-parser"        % circeVersion,
      "com.github.pureconfig" %% "pureconfig-core"     % pureConfigVersion,
      "org.yaml"               % "snakeyaml"           % snakeyamlVersion,
      "org.http4s"            %% "http4s-core"         % http4sVersion,
      "org.http4s"            %% "http4s-ember-client" % http4sVersion,
      "org.http4s"            %% "http4s-circe"        % http4sVersion
    )
  )

lazy val oracles = (project in file("oracles"))
  .dependsOn(core)
  .settings(name := "cryptosis-oracles")

lazy val providers = (project in file("providers"))
  .dependsOn(core)
  .settings(name := "cryptosis-providers")

lazy val exchanges = (project in file("exchanges"))
  .dependsOn(core)
  .settings(name := "cryptosis-exchanges")

lazy val cryptosis = (project in file("cryptosis"))
  .settings(
    name := "cryptosis-core",
    libraryDependencies ++= Seq(
      "org.typelevel"            %% "cats-core"           % catsVersion,
      "org.typelevel"            %% "cats-effect"         % catsEffectVersion,
      "io.circe"                 %% "circe-core"          % circeVersion,
      "io.circe"                 %% "circe-parser"        % circeVersion,
      "io.circe"                 %% "circe-generic"       % circeVersion,
      "org.http4s"               %% "http4s-core"         % http4sVersion,
      "org.http4s"               %% "http4s-ember-client" % http4sVersion,
      "org.http4s"               %% "http4s-circe"        % http4sVersion,
      "org.http4s"               %% "http4s-dsl"          % http4sVersion,
      "com.github.pureconfig"    %% "pureconfig-core"     % pureConfigVersion,
      "org.yaml"                  % "snakeyaml"           % snakeyamlVersion,
      ("com.github.vickumar1981" %% "stringdistance"      % stringdistVersion).cross(
        CrossVersion.for3Use2_13
      )
    )
  )

lazy val testKit = (project in file("testKit"))
  .dependsOn(cryptosis)
  .settings(
    name := "cryptosis-testkit",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"               % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck"    % munitVersion % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7"      % Test
    )
  )

lazy val root = (project in file("."))
  .dependsOn(cryptosis)
  .settings(name := "cryptosis")
