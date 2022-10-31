package com.bjoru.cryptosis

import sbt._
import sbt.Keys._

import java.io.IOException
import java.nio.file.Path
import java.nio.charset.StandardCharsets

import java.nio.file.{Path, Files}

import scala.jdk.CollectionConverters._
import org.web3j.codegen.SolidityFunctionWrapperGenerator

final case class Contract(file: File, srcDir: File, resourceDir: File) {
  val packageName: String = "com.bjoru.cryptosis.contracts"

  val abiFile: File = resourceDir / replaceExt(file, ".abi")
  val binFile: File = resourceDir / replaceExt(file, ".bin")
  val outFile: File = srcDir / replaceExt(file, ".java")

  def solidityGeneratorArgs: Array[String] = Array(
    file.absolutePath,
    "--bin",
    "--abi",
    "--optimize",
    "-o", resourceDir.absolutePath
  )

  def web3jGeneratorArgs: Array[String] = Array(
    "-b", binFile.absolutePath,
    "-a", abiFile.absolutePath,
    "-o", srcDir.absolutePath,
    "-p", packageName
  )

  def replaceExt(file: File, ext: String): String =
    file.getName.replaceFirst("[.][^.]+$", ext)
}

object Contract {

  val filter = new java.io.FileFilter {
    def accept(f: File): Boolean =
      f.getName.endsWith(".sol")
  }

  def scan(baseDir: File, srcDir: File, resourceDir: File): Seq[Contract] = {
    baseDir.listFiles(filter).map(Contract(_, srcDir, resourceDir))
  }
}

object SolidityGenerator {

  val cmd = "solc"

  def generate(contract: Contract): Contract = ???

  def generateAll(contracts: Seq[Contract]): Seq[Contract] =
    contracts.map(generate)
}

object Web3JGenerator {

  def generate(contract: Contract): Contract = {
    SolidityFunctionWrapperGenerator.main(contract.web3jGeneratorArgs)
    contract
  }

  def generateAll(contract: Seq[Contract]): Seq[Contract] = 
    contract.map(generate)
}

// Step1: solc <contract>.sol --bin --abi --optimize -o <output-dir>/
// Step2: web3j generate solidity -b /path/to/sc.bin -a /path/to/cs.abi -o /path/to/src/main/java -p com.bjoru.cryptosis.contracts
// fail gracefully (with warning) if solc not on path
object ContractGeneratorPlugin extends AutoPlugin {

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val solidityGen = taskKey[Seq[Path]]("Generate abi and bin files from solidity files")
    val web3jGen    = taskKey[Seq[Path]]("Generate java interfaces for solidity files.")
  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq( //inConfig(Compile) {
    solidityGen / fileInputs += baseDirectory.value.toGlob / ** / ".sol",
    solidityGen := {
      val outputDir = Files.createDirectories(streams.value.cacheDirectory.toPath)
      val logger    = streams.value.log

      def compile(path: Path): (Path, Path) = {
        val binPath = outputDir / path.getFileName.toString.replaceAll(".sol", ".bin")
        val abiPath = outputDir / path.getFileName.toString.replaceAll(".sol", ".abi")

        logger.info(s"Compiling $path")
        Seq("solc", "--bin", "--abi", "--optimize", "-o", outputDir).!!

        binPath -> abiPath
      }
    }

    //Compile / resourceGenerators += runSolidityGen((Compile / resourceManaged).value).taskValue,
    //Compile / sourceGenerators   += runWeb3jGen((Compile / sourceManaged).value).taskValue
  )

  def allContracts(): Seq[Contract] = ???

  def runSolidityGen(path: File): Seq[File] = ???

  def runWeb3jGen(path: File): Seq[File] = ???

    //sourceGenerators in Compile += Def.task {
      //lazy val sources = generateWrapper.value
      //streams.value.log.info("Generated sources:\r\n" + sources.mkString("\r\n"))
      //sources
    //}.taskValue
  //} ++ pluginSettings


  lazy val generateWrapper: Def.Initialize[Task[Seq[File]]] = Def.task {
    Generate(web3jContractPath.value, web3jUseNativeTypes.value, web3jOutputPath.value)
  }
}

object Generate {

  def apply(contractsPath: File, nativeTypes: Boolean, outputPath: File): Seq[File] = {
    val contracts = AbiBin.findList(contractsPath)
    println(s"Found: ${contracts}")
    process(contracts, nativeTypes, outputPath)
  }

  def process(contracts: Seq[AbiBin], nativeTypes: Boolean, outputDir: File): Seq[File] = {
    val files = contracts.map { contract =>
      try {
        val generator = new SolidityFunctionWrapperGenerator(
          contract.bin.toFile, 
          contract.abi.toFile, 
          outputDir, 
          contract.name,
          contract.packageName, 
          nativeTypes,
          false,
          20
        )

        generator.generate()

        Some(contract.newLocation(outputDir))
      } catch {
        case e @ (_: IOException | _: ClassNotFoundException) =>
          println(e.getMessage)
          None
      }
    }

    files.flatten[File]
  }

  def readFile(path: Path): String =
    new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
}

