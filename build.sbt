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

lazy val bitcoinApi = (project in file("chains/bitcoin"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-bitcoin")

lazy val ethereumApi = (project in file("chains/ethereum"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-ethereum")

lazy val binanceApi = (project in file("chains/binance"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-binance")

lazy val avalancheApi = (project in file("chains/avalanche"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-avalanche")

lazy val polygonApi = (project in file("chains/polygon"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-polygon")

lazy val fantomApi = (project in file("chains/fantom"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-fantom")

lazy val solanaApi = (project in file("chains/solana"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-solana")

lazy val elrondApi = (project in file("chains/elrond"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-elrond")

lazy val dogecoinApi = (project in file("chains/dogecoin"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-dogecoin")

lazy val harmonyApi = (project in file("chains/harmony"))
  .dependsOn(core)
  .settings(name := "crypto-scanner-harmony")

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
      "io.circe"              %% "circe-parser"      % circeVersion,
      "io.circe"              %% "circe-generic"   % circeVersion,
      "org.http4s"            %% "http4s-core"     % http4sVersion,
      "org.http4s"            %% "http4s-ember-client" % http4sVersion,
      "org.http4s"            %% "http4s-circe"        % http4sVersion,
      "org.http4s"            %% "http4s-dsl"          % http4sVersion,
      "com.github.pureconfig" %% "pureconfig-core" % pureConfigVersion,
      "org.yaml"               % "snakeyaml"       % snakeyamlVersion,
      ("com.github.vickumar1981" %% "stringdistance" % stringdistVersion).cross(CrossVersion.for3Use2_13)
    )
  )

lazy val testKit = (project in file("testKit"))
  .dependsOn(ethereumApi, elrondApi, bitcoinApi, dogecoinApi, solanaApi)
  .settings(
    name := "crypto-scanner-testkit",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"            % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitVersion % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
    )
  )

lazy val root = (project in file("."))
  .dependsOn(
    bitcoinApi, ethereumApi, solanaApi, elrondApi, dogecoinApi, avalancheApi,
    binanceApi, polygonApi, fantomApi, harmonyApi
  )
  .settings(
    name := "crypto-scanner",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    )
  )
