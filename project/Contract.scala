package com.bjoru.cryptosis

import sbt._

import java.nio.file.Path

final case class Contract(solidityFile: Path, subpath: String, outDir: Path) {

  val outputDir: Path = outDir.resolve(subpath).getParent

  val abiPath: Path = outputDir / replaceExt(solidityFile, ".abi")
  val binPath: Path = outputDir / replaceExt(solidityFile, ".bin")

  val packageName: String = {
    val xs = subpath.split(java.io.File.separatorChar).dropRight(1)
    "com.bjoru.cryptosis." + xs.map(_.toLowerCase).mkString(".")
  }

  def sourceFile(baseDir: File): File = {
    val x = baseDir.toPath.resolve(packageName.replaceAllLiterally(".", "/"))
    x.resolve(replaceExt(solidityFile, ".java")).toFile
  }

  private def replaceExt(file: Path, newExt: String): String =
    file.toFile.getName.replaceFirst("[.][^.]+$", newExt)

}
