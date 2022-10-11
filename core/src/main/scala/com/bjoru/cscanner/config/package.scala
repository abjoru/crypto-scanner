package com.bjoru.cscanner.config

import cats.effect.IO
import pureconfig.*

import com.bjoru.cscanner.Chain
import com.bjoru.cscanner.types.{Wallet, Token}

import scala.reflect.ClassTag

import java.nio.file.Path

given [T, U](using ConfigReader[T], ConfigReader[U]): ConfigReader[Map[T, Seq[U]]] = ???

def loadYamlFile[T: ClassTag](path: Path)(using ConfigReader[T]): IO[T] =
  for src <- IO.pure(YamlConfigSource.file(path))
      res <- IO.fromEither(src.load[T].left.map(error.ConfigReaderException(_)))
  yield res

def loadWallets(path: Path): IO[List[Wallet]] =
  loadYamlFile[List[Wallet]](path)

def loadTokens(path: Path): IO[Map[Chain, Seq[Token]]] =
  loadYamlFile[Map[Chain, Seq[Token]]](path)
