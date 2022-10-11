package com.bjoru.cscanner.config

import cats.syntax.traverse.given
import cats.effect.IO
import pureconfig.*

import com.bjoru.cscanner.{Chain, given}
import com.bjoru.cscanner.types.{Wallet, Token, Provider, Endpoint}

import scala.reflect.ClassTag
import scala.collection.JavaConverters.given

import java.util.{Map as JMap, List as JList}
import java.nio.file.Path

def loadYamlFile[T: ClassTag](path: Path)(using ConfigReader[T]): IO[T] =
  for src <- IO.pure(YamlConfigSource.file(path))
      res <- IO.fromEither(src.load[T].left.map(error.ConfigReaderException(_)))
  yield res

def loadWallets(path: Path): IO[List[Wallet]] =
  loadYamlFile[List[Wallet]](path)

def loadTokens(path: Path): IO[Map[Chain, Seq[Token]]] =
  loadYamlFile[Map[Chain, Seq[Token]]](path)

def loadEndpoints(path: Path): IO[Map[Provider, Seq[Endpoint]]] =
  loadYamlFile[Map[Provider, Seq[Endpoint]]](path)
