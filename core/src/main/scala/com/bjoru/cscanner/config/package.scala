package com.bjoru.cscanner.config

import cats.syntax.traverse.given
import cats.effect.IO
import pureconfig.*
import io.circe.Decoder
import io.circe.parser.*

import com.bjoru.cscanner.{Chain, given}
import com.bjoru.cscanner.types.{Wallet, Token, Provider, Endpoint}

import scala.reflect.ClassTag
import scala.collection.JavaConverters.given
import scala.io.Source

import java.util.{Map as JMap, List as JList}
import java.nio.file.Path

def loadYamlFile[T: ClassTag](path: Path)(using ConfigReader[T]): IO[T] =
  for src <- IO.pure(YamlConfigSource.file(path))
      res <- IO.fromEither(src.load[T].left.map(error.ConfigReaderException(_)))
  yield res

def loadJsonFile[T](path: Path)(using Decoder[T]): IO[T] =
  for content <- IO(Source.fromFile(path.toFile).getLines.mkString("\n"))
      json    <- IO.fromEither(parse(content))
      result  <- IO.fromEither(json.as[T])
  yield result

def loadWallets(path: Path): IO[List[Wallet]] =
  loadYamlFile[List[Wallet]](path)

def loadTokens(path: Path): IO[Map[Chain, Seq[Token]]] =
  loadYamlFile[Map[Chain, Seq[Token]]](path)

def loadEndpoints(path: Path): IO[Map[Provider, Seq[Endpoint]]] =
  loadYamlFile[Map[Provider, Seq[Endpoint]]](path)

def findEndpoint(path: Path, provider: Provider, chain: Chain): IO[Endpoint] =
  for all <- loadEndpoints(path)
      res  = all.get(provider).flatMap(_.find(_.chain == chain))
      rio <- res.fold(IO.raiseError(new Exception(s"No $provider for $chain")))(IO.pure(_))
  yield rio
