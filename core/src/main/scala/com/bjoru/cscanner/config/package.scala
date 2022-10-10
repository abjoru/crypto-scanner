package com.bjoru.cscanner.config

import cats.effect.IO
import pureconfig.*

import com.bjoru.cscanner.types.{Wallet, Token}

import scala.reflect.ClassTag

import java.nio.file.Path

def loadConfigFile[T: ClassTag](path: Path)(using ConfigReader[T]): IO[T] =
  for src <- IO.pure(ConfigSource.file(path))
      res <- IO.fromEither(src.load[T].left.map(error.ConfigReaderException(_)))
  yield res

def loadWallets(path: Path): IO[List[Wallet]] =
  loadConfigFile[List[Wallet]](path)

def loadTokens(path: Path): IO[List[Token]] =
  loadConfigFile[List[Token]](path)

/*
def loadAllChains(path: Path): IO[List[ChainApi]] =
  loadConfigFile[List[ChainApi]](path)

def loadChain(chain: String, path: Path): IO[ChainApi] =
  loadAllChains(path).flatMap { xs =>
    xs.find(_.chain.trim.toLowerCase == chain.toLowerCase) match
      case Some(ch) => IO.pure(ch)
      case None     => IO.raiseError(new Exception(s"Missing chain: $chain in $path"))
  }
*/
