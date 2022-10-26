package com.bjoru.cryptosis

import cats.effect.IO

import io.circe.{HCursor, DecodingFailure as DF}
import io.circe.Decoder.Result

import com.bjoru.cryptosis.syntax.*

import scala.concurrent.duration.*
import scala.util.Try

import java.io.File
import java.nio.file.{Path, Paths}

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

def cryptosisDirectory(xdg: Xdg): FilePath = 
  getXdgDirectory(xdg) </> "cryptosis"

def putStrLn(str: String): IO[Unit] = IO(println(str))

////////////////
// Extensions //
////////////////

extension (v: String)
  def </>(o: String): FilePath = Paths.get(v, o)

extension (v: File)
  def </>(o: String): FilePath = v.toPath.resolve(o)

extension (v: Path)
  def </>(o: String): FilePath = v.resolve(o)
  def exists: Boolean = v.toFile.exists()
  def delete: IO[Boolean] = PathOps.delete(v)
  def expired(duration: FiniteDuration): Boolean = PathOps.expired(v, duration)
  def mkdirs: IO[Boolean] = PathOps.mkdirs(v)

extension [T](t: Try[T])
  def toCirce(hc: HCursor): Result[T] =
    t.toEither.left.map(e => DF.fromThrowable(e, hc.history))

extension [T](t: Option[T])
  def toCirce: Result[T] =
    t.fold(Left(DF("empty field!", List.empty)))(Right(_))

/////////////////
// Typeclasses //
/////////////////

trait Identity[T]:
  extension (t: T) def id: types.Id
