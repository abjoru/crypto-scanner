package com.bjoru.cryptosis

import io.circe.{HCursor, DecodingFailure as DF}
import io.circe.Decoder.Result

import scala.util.Try

import java.nio.file.{Path, Paths}

import java.io.File

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

extension (v: String)
  def </>(o: String): FilePath = Paths.get(v, o)

extension (v: File)
  def </>(o: String): FilePath = v.toPath.resolve(o)

extension (v: Path)
  def </>(o: String): FilePath = v.resolve(o)

extension [T](t: Try[T])
  def toCirceResult(c: HCursor): Result[T] =
    t.toEither.left.map(e => DF.fromThrowable(e, c.history))

/////////////////
// Typeclasses //
/////////////////

trait Identity[T]:
  extension (t: T) def id: types.Id
