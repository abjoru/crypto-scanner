package com.bjoru.cryptosis.config

import cats.effect.IO

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import pureconfig.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.types.*
import com.bjoru.cryptosis.instances.given

import scala.reflect.ClassTag
import scala.io.Source

import java.io._

def loadYamlFile[T: ClassTag](file: FilePath)(using ConfigReader[T]): IO[T] =
  for src <- IO.pure(YamlConfigSource.file(file))
      res <- IO.fromEither(src.load[T].left.map(error.ConfigReaderException(_)))
  yield res

def loadJsonFile[T](file: FilePath)(using Decoder[T]): IO[T] =
  for cont <- IO(Source.fromFile(file.toFile).getLines.mkString("\n"))
      json <- IO.fromEither(parse(cont))
      res  <- IO.fromEither(json.as[T])
  yield res

def saveJsonFile[T](file: FilePath, data: T)(using Encoder[T]): IO[Unit] = 
  for dir <- file.mkdirs
      wr   = BufferedWriter(FileWriter(file.toFile))
      _   <- IO(wr.write(data.asJson.spaces2))
      _   <- IO(wr.close)
  yield ()
