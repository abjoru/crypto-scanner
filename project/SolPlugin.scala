package com.bjoru.cryptosis

import sbt._
import sbt.Keys._
import sbt.nio._
import sbt.nio.Keys._

import java.nio.file.{Files, Path}

import org.web3j.codegen.{SolidityFunctionWrapperGenerator => SolGen}

import scala.sys.process._

object SolPlugin extends AutoPlugin {

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val solidityFiles = settingKey[Seq[String]]("Solidity files to generate wrappers for")

    val solidityGen = taskKey[Seq[File]]("Generate abi/bin files from solidity files")
  }

  import autoImport._

  override def projectSettings: Seq[Setting[_]] = Seq(
    solidityGen := {
      val outputDir = (Compile / resourceManaged).value.toPath
      val logger = streams.value.log

      val contracts = solidityFiles.value.map { subpath =>
        val base = baseDirectory.value.toPath / "src" / "main" / "contracts"
        val fullpath = base.resolve(subpath)
        Contract(fullpath, subpath, outputDir)
      }

      logger.info(s"Compiling ${contracts.size} solidity files")

      val compiled = contracts.map { c =>
        logger.info(s"Compiling ${c.solidityFile} to ${c.outputDir}")

        Seq("solc", "--bin", "--abi", "--optimize", "--overwrite", "-o", c.outputDir.toString, c.solidityFile.toString).!!
        c
      }

      compiled.map { c =>
        val args = Array(
          "-b", c.binPath.toFile.absolutePath,
          "-a", c.abiPath.toFile.absolutePath,
          "-o", ((Compile / sourceManaged).value / "java").absolutePath,
          "-p", c.packageName
        )

        SolGen.main(args)

        c.sourceFile((Compile / sourceManaged).value / "java")
      }
    },
    Compile / sourceGenerators += solidityGen.taskValue
  )
}
