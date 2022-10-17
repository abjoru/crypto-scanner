package com.bjoru.cryptos.config

import cats.effect.IO

import io.circe.Decoder
import io.circe.parser.*
import pureconfig.*

import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.instances.given

import scala.reflect.ClassTag
import scala.io.Source

import java.nio.file.Path

def loadYamlFile[T: ClassTag](path: Path)(using ConfigReader[T]): IO[T] =
  for src <- IO.pure(YamlConfigSource.file(path))
      res <- IO.fromEither(src.load[T].left.map(error.ConfigReaderException(_)))
  yield res

def loadJsonFile[T](path: Path)(using Decoder[T]): IO[T] =
  for cont <- IO(Source.fromFile(path.toFile).getLines.mkString("\n"))
      json <- IO.fromEither(parse(cont))
      res  <- IO.fromEither(json.as[T])
  yield res
