package com.bjoru.cryptosis

import sbt._
import sbt.Keys._
import sbt.nio._
import sbt.nio.Keys._

import _root_.io.circe._
import _root_.io.circe.parser.parse

import java.io.File
import java.nio.file.{Files, Path}

import org.web3j.codegen.{SolidityFunctionWrapperGenerator => SolGen}

import scala.util.Try
import scala.io.Source
import scala.sys.process._
import java.io.BufferedWriter
import java.io.FileWriter

object Web3Plugin extends AutoPlugin {

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val web3Contracts   = settingKey[Seq[String]]("List of contract paths to generate")
    val web3Packagename = settingKey[String]("Base package name for generated resources")
    val web3Generate    = taskKey[Seq[File]]("Generates Java wrappers for contracts")
  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    web3Contracts := Seq.empty[String],
    web3Packagename := "com.bjoru.cryptosis.contracts.gen",
    web3Generate  := {
      val env = GenEnv(
        rootPath        = baseDirectory.value.toPath,
        sourceManaged   = (Compile / sourceManaged).value.toPath,
        resourceManaged = (Compile / resourceManaged).value.toPath,
        packageBasename = web3Packagename.value,
        logger          = streams.value.log
      )

      val contracts = web3Contracts.value.map(v => EthContract(v, env))

      compileContracts(contracts, env)
      generateWrappers(contracts, env)
    },

    Compile / sourceGenerators += web3Generate.taskValue
  )

  private def compileContracts(contracts: Seq[EthContract], env: GenEnv): Unit = {
    env.logger.info(s"Starting work in: ${env.rootPath}")
    val cmd = Seq("npx", "hardhat", "compile")
    Process(cmd, env.rootPath.toFile).!!
    contracts.foreach { contract =>
      env.logger.info(s"Generating abi/bin for ${contract.assetFile}")
      if (!contract.abiPath.toFile.exists) {
        contract.getOutputFiles.get.foreach { f =>
          val outputFile = f.outputFile.toFile
          outputFile.getParentFile.mkdirs
          IO.write(outputFile, f.contents)
        }
      }
    }
  }

  private def generateWrappers(contracts: Seq[EthContract], env: GenEnv): Seq[File] = {
    contracts.foldLeft(Seq.empty[File]) {
      case (acc, contract) =>
        val args = Array(
          "-b", contract.binPath.toFile.absolutePath,
          "-a", contract.abiPath.toFile.absolutePath,
          "-o", env.sourceManaged.toFile.absolutePath,
          "-p", env.packageBasename
        )

        SolGen.main(args)

        acc :+ contract.javaOutputFile
    }
  }

}

final case class GenEnv(
  rootPath: Path,
  sourceManaged: Path,
  resourceManaged: Path,
  packageBasename: String,
  logger: Logger
)

final case class CFile(
  outputFile: Path,
  contents: String
)

final case class EthContract(inputPath: String, env: GenEnv) {

  val assetFile = (env.rootPath / "artifacts").resolve(inputPath).resolve(fileBasename + ".json")

  lazy val abiPath: Path = outputPath(".abi")
  lazy val binPath: Path = outputPath(".bin")

  lazy val javaOutputDir: Path = 
    env.sourceManaged.resolve(env.packageBasename.split('.').mkString(File.separator))

  lazy val javaOutputFile: File = 
    (javaOutputDir / replaceExt(inputPath, ".java")).toFile

  def getOutputFiles(): Try[Seq[CFile]] = {
    val src  = Source.fromFile(assetFile.toFile).getLines.mkString("\n")

    parse(src).flatMap { json =>
      for {
        abi <- json.hcursor.downField("abi").as[Json]
        bin <- json.hcursor.downField("bytecode").as[String]
        res  = if (bin.startsWith("0x")) bin.drop(2) else bin
      } yield Seq(CFile(abiPath, abi.spaces2), CFile(binPath, res))
    }.toTry
  }

  private def fileBasename: String = replaceExt(inputPath.split(File.separatorChar).last, "")

  private def replaceExt(file: String, ext: String): String =
    file.split(File.separator).last.replaceFirst("[.][^.]+$", ext)

  private def outputPath(ext: String): Path = {
    val basePath = inputPath.split(File.separatorChar).dropRight(1)
    val base = basePath.mkString(File.separator).replaceAllLiterally("@", "")
    env.resourceManaged.resolve(base) / replaceExt(inputPath, ext)
  }
  
}
