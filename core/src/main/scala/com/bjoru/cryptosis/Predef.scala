package com.bjoru.cryptosis

import cats.Monoid
import cats.data.StateT
import cats.effect.IO

import io.circe.{Decoder, Encoder, HCursor, DecodingFailure as DF}
import io.circe.Decoder.Result
import io.circe.parser.parse
import io.circe.syntax.*

import pureconfig.ConfigReader
import pureconfig.error.ConfigReaderException

import com.bjoru.cryptosis.config.YamlConfigSource
import com.bjoru.cryptosis.syntax.*

import scala.io.Source
import scala.concurrent.duration.*
import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag

import java.io.{File, BufferedWriter, FileWriter}
import java.nio.file.{Path, Paths}

type FilePath = Path

type SIO[T] = StateT[IO, State, T]

object SIO:

  def apply[T](f: State => IO[(State, T)]): SIO[T] = StateT.apply(f)
  def applyF[T](runF: IO[State => IO[(State, T)]]): SIO[T] = StateT.applyF(runF)
  def get: SIO[State] = StateT.get
  def inspect[T](f: State => T): SIO[T] = StateT.inspect(f)
  def inspectF[T](f: State => IO[T]): SIO[T] = StateT.inspectF(f)
  def liftF[T](io: IO[T]): SIO[T] = StateT.liftF(io)
  def modify(f: State => State): SIO[Unit] = StateT.modify(f)
  def modifyF(f: State => IO[State]): SIO[Unit] = StateT.modifyF(f)
  def pure[T](v: T): SIO[T] = StateT.pure(v)
  def set(e: State): SIO[Unit] = StateT.set(e)
  def setF(ioe: IO[State]): SIO[Unit] = StateT.setF(ioe)

  def fromTry[T](v: Try[T]): SIO[T] = SIO.liftF(IO.fromTry(v))
  def fromOption[T](v: Option[T])(orElse: => Throwable): SIO[T] = liftF(IO.fromOption(v)(orElse))

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

def loadYaml[T: ClassTag](file: FilePath)(using ConfigReader[T]): IO[T] =
  for src <- IO.pure(YamlConfigSource.file(file))
      res <- IO.fromEither(src.load[T].left.map(ConfigReaderException(_)))
  yield res

def loadJson[T](file: FilePath)(using Decoder[T]): IO[T] =
  for src  <- IO(Source.fromFile(file.toFile).getLines.mkString("\n"))
      json <- IO.fromEither(parse(src))
      res  <- IO.fromEither(json.as[T])
  yield res

def loadJsonOrElse[T](file: FilePath)(orElse: => T)(using Decoder[T]): IO[T] =
  if file.exists
    then loadJson(file)
    else IO.pure(orElse)

def saveJson[T](file: FilePath, data: T)(using Encoder[T]): IO[Unit] =
  for dir <- file.mkdirs
      bwr  = BufferedWriter(FileWriter(file.toFile))
      _   <- IO(bwr.write(data.asJson.spaces2))
      _   <- IO(bwr.close)
  yield ()

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
  def toIO: IO[T] = t match
    case Success(a) => IO.pure(a)
    case Failure(e) => IO.raiseError(e)
  def toSIO: SIO[T] = SIO.liftF(t.toIO)

extension [T](t: Either[Throwable, T])
  def toIO: IO[T] = t match
    case Right(a) => IO.pure(a)
    case Left(er) => IO.raiseError(Exception(er.getMessage.take(500)))
  def toSIO: SIO[T] = SIO.liftF(t.toIO)

extension [T](t: Option[T])
  def toCirce: Result[T] =
    t.fold(Left(DF("empty field!", List.empty)))(Right(_))
  def toCirce(msg: String): Result[T] =
    t.fold(Left(DF(msg, List.empty)))(Right(_))
  def toIO: IO[T] = t match
    case Some(a) => IO.pure(a)
    case None    => IO.raiseError(Exception("Empty field!"))
  def toSIO: SIO[T] = SIO.liftF(t.toIO)

extension [T](io: IO[T])
  def withErrorHandler: IO[T] = 
    io.handleErrorWith {
      case err => IO.raiseError(Exception(err.getMessage.take(500)))
    }

/////////////////
// Typeclasses //
/////////////////

trait Identity[T]:
  extension (t: T) def id: types.Id
