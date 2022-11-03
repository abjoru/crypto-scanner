// Local build of web3j (local maven repo)
lazy val web3jVersion = "4.9.5-SNAPSHOT"
lazy val circeVersion = "0.14.1"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "org.web3j" % "abi" % web3jVersion,
  "org.web3j" % "core" % web3jVersion,
  "org.web3j" % "crypto" % web3jVersion,
  "org.web3j" % "codegen" % web3jVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion
)
