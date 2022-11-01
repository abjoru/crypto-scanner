package com.bjoru.cryptosis

import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._

import scala.sys.process

object NpmPlugin extends AutoPlugin {

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val npm                = inputKey[Unit]("Run an npm commaon")
    val npmWorkingDir      = settingKey[String]("npm working directory")
    val npmCompileCommands = settingKey[String]("npm commands to run during compile phase")
    val npmTestCommands    = settingKey[String]("npm commands to run during test phase")
    val npmCleanCommands   = settingKey[String]("npm commands to run during clean phase")
  }

  import autoImport._

  def runNpm(commands: Seq[String], wd: String, logger: Logger): Unit = {
    val npmCommand = s"npm ${commands.mkString(" ")}"
    logger.info(s"Running '$npmCommand' in $wd")
    val rc = process.Process(npmCommand, file(wd)).!
    if (rc != 0) {
      val msg = s"$npmCommand returned non-zero return code: $rc"
      logger.error(msg)
      sys.error(msg)
    }
  }

  override def projectSettings: Seq[Setting[_]] = Seq(
    npmWorkingDir := ".",
    npmCompileCommands := "",
    npmTestCommands := "",
    npmCleanCommands := "",

    npm := runNpm(spaceDelimited("<arg>").parsed, npmWorkingDir.value, streams.value.log),

    Compile / compile := {
      runNpm(Seq(npmCompileCommands.value), npmWorkingDir.value, streams.value.log)
      (Compile / compile).value
    }
  )
}
