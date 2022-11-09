import Dependencies._

ThisBuild / organization := "com.bjoru"
ThisBuild / version      := "0.1"
ThisBuild / scalaVersion := "3.2.0"
ThisBuild / resolvers    += Resolver.mavenLocal

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
      "org.http4s"            %% "http4s-circe"        % http4sVersion,
      "org.http4s"               %% "http4s-dsl"          % http4sVersion,
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

/////////////////////
// Smart contracts //
/////////////////////

lazy val ethereum = (project in file("contracts/ethereum"))
  .enablePlugins(Web3Plugin)
  .dependsOn(core)
  .settings(
    name := "cryptosis-contracts-ethereum",
    libraryDependencies ++= Seq(
      "org.web3j" % "core" % web3jVersion
    ),
    web3Contracts := Seq(
      "@openzeppelin/contracts/token/ERC20/ERC20.sol",
      "contracts/mc/View.sol"
    )
  )

lazy val testKit = (project in file("testKit"))
  .dependsOn(oracles, ethereum, providers)
  .settings(
    name := "cryptosis-testkit",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"               % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck"    % munitVersion % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7"      % Test
    )
  )

lazy val root = (project in file("."))
  .dependsOn(core, oracles, providers, exchanges, ethereum)
  .settings(name := "cryptosis")
