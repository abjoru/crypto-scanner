package com.bjoru.cryptos.types

import cats.effect.IO
import org.http4s.Uri

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptos.instances.given
import com.bjoru.cryptos.config.*

import scala.util.{Try, Success, Failure}

import java.nio.file.Path

enum Provider:
  case Zapper
  case Zerion
  case CovalentHQ
  case BlockCypher
  case QuickNode

object Provider:

  given ConfigReader[Provider] = ConfigReader.fromString { str =>
    Try(Provider.valueOf(str)) match
      case Success(p) => Right(p)
      case Failure(e) => Left(ExceptionThrown(e))
  }

final case class Endpoint(
  uri: Uri,
  apiKey:  String
) derives ConfigReader

object Endpoint:

  def loadAll(path: Path): IO[Map[Provider, Endpoint]] =
    loadYamlFile[Map[Provider, Endpoint]](path)

  def find(path: Path, provider: Provider): IO[Endpoint] =
    for all <- loadAll(path)
        res <- IO.fromOption(all.get(provider))(new Exception(s"Missing $provider!"))
    yield res
