package com.bjoru.cryptosis

import java.io.File
import java.nio.file.{Path, Paths, Files}
import java.util.regex.Pattern

import scala.jdk.CollectionConverters._
import java.io.{IOException, UncheckedIOException}

case class AbiBin(packageWithFilename: String, abiBinPath: Path, searchBase: Path) {
  lazy val name = AbiBin.getFilenameFromPackageName(packageWithFilename)
  lazy val packageName = getPackageNameFromPackageWithFilename(packageWithFilename)
  lazy val basePath = abiBinPath.getParent
  lazy val abi = basePath.resolve(name + AbiBin.AbiExtension)
  lazy val bin = basePath.resolve(name + AbiBin.BinExtension)

  def isValid = Files.isRegularFile(abi) && Files.isRegularFile(bin)
  def newLocation(out: File) = out.toPath.resolve(relativePath).resolve(name + ".java").toFile

  private lazy val relativePath = {
    val path = searchBase.relativize(basePath)
    val initialPathname = searchBase.getName(searchBase.getNameCount - 1)

    if (Files.isSameFile(searchBase.toAbsolutePath, basePath.toAbsolutePath)) initialPathname
    else initialPathname.resolve(path)
  }

  private def getPackageNameFromPackageWithFilename(pgk: String) =
    packageWithFilename.split(Pattern.quote(".")).dropRight(1).mkString(".")
}

object AbiBin {
  val AbiExtension = ".abi"
  val BinExtension = ".bin"
  val ExtensionLength = 4

  def findList(f: File): Seq[AbiBin] = findListImpl(f.toPath)

  def findList(p: Path): Seq[AbiBin] = findListImpl(p)

  def convertPathToPackageWithFilename(p: Path) = {
    val nameCount = p.getNameCount
    val pathNames = for (index <- 0 until nameCount) yield {
      val name = p.getName(index)
      if (index == nameCount - 1) Paths.get(removeExtension(name.toString))
      else name
    }

    pathNames.mkString(".")
  }

  def getFilenameFromPackageName(p: String) =
    p.split(Pattern.quote(".")).last

  private def hasCorrectExtension(p: Path) =
    p.getFileName.toString.endsWith(AbiExtension) || p.getFileName.toString.endsWith(BinExtension)

  private def findListImpl(basePath: Path) = {
    val baseSearchPath = basePath.toAbsolutePath
    println(s"basepath exists: ${baseSearchPath}")

    try {
      val pf: PartialFunction[Path, (String, Path)] = {
        case p: Path =>
          val rpath = baseSearchPath.getParent.relativize(p)
          convertPathToPackageWithFilename(rpath) -> p
      }

      Files.walk(baseSearchPath)
           .iterator()
           .asScala
           .filter(hasCorrectExtension)
           .collect(pf)
           .toSeq
           .groupBy(_._1)
           .mapValues(_.map(_._2))
           .filter(k => k._2.lengthCompare(2) == 0)
           .map(e => new AbiBin(e._1, e._2.head, baseSearchPath))
           .toSeq
    } catch {
      case e: IOException => throw new UncheckedIOException(e)
    }
  }

  private def removeExtension(name: String) =
    name.substring(0, name.length - ExtensionLength)
}
