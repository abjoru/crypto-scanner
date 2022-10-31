lazy val web3jVersion = "4.9.4"

libraryDependencies ++= Seq(
  "org.web3j" % "abi" % web3jVersion,
  "org.web3j" % "core" % web3jVersion,
  "org.web3j" % "crypto" % web3jVersion,
  "org.web3j" % "codegen" % web3jVersion
)
