package com.bjoru.cryptosis

import cats.effect.IO

import io.circe.{HCursor, DecodingFailure as DF}
import io.circe.Decoder.Result

import scala.util.Try
import scala.concurrent.duration.*
import scala.jdk.javaapi.DurationConverters.toJava

import java.nio.file.{Path, Paths}

import java.io.File
import java.time.Instant
import java.time.temporal.TemporalUnit
import java.time.LocalDateTime
import java.time.ZoneId

export config.{loadYamlFile, loadJsonFile, saveJsonFile}
export config.{Endpoint, TokenFilter, Provider}

//object Predef:

type FilePath = Path

enum Xdg:
  case Data
  case Config
  case Cache
  case State

lazy val homeDir = System.getProperty("user.home")

def getXdgDirectory(xdg: Xdg): FilePath = xdg match
  case Xdg.Data   => Paths.get(homeDir, ".local", "share")
  case Xdg.Config => Paths.get(homeDir, ".config")
  case Xdg.Cache  => Paths.get(homeDir, ".cache")
  case Xdg.State  => Paths.get(homeDir, ".local", "state")

def getHomeDirectory: FilePath = Paths.get(homeDir)

def putStrLn(str: String): IO[Unit] = IO(println(str))

extension (v: String)
  def </>(o: String): FilePath = Paths.get(v, o)

extension (v: File)
  def </>(o: String): FilePath = v.toPath.resolve(o)

extension (v: Path)

  def </>(o: String): FilePath = v.resolve(o)

  def exists: Boolean = v.toFile.exists()

  def delete: Boolean = v.toFile.delete()

  def expired(duration: FiniteDuration): Boolean = v.exists match
    case true if v.toFile.isFile =>
      val ts = LocalDateTime.ofInstant(Instant.ofEpochMilli(v.toFile.lastModified), ZoneId.systemDefault)
      val target = ts.plus(toJava(duration))
      target.isBefore(LocalDateTime.now)
    case _ => true

  def mkdirs: IO[Boolean] = v.exists match
    case false if v.toFile.isDirectory  => IO(v.toFile.mkdirs())
    case false                          => IO(v.toFile.getParentFile.mkdirs())
    case true                           => IO.pure(false)

extension [T](t: Try[T])
  def toCirceResult(c: HCursor): Result[T] =
    t.toEither.left.map(e => DF.fromThrowable(e, c.history))

extension [T](t: Option[T])
  def toCirceResult: Result[T] =
    t.fold(Left(DF("empty field!", List.empty)))(Right(_))

/////////////////
// Typeclasses //
/////////////////

trait Identity[T]:
  extension (t: T) def id: types.Id
