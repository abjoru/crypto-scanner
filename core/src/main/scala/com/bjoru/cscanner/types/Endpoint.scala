package com.bjoru.cscanner.types

import pureconfig.ConfigReader
import pureconfig.error.ExceptionThrown
import pureconfig.generic.derivation.default.*

import org.http4s.Uri

import com.bjoru.cscanner.{Chain, given}

import scala.util.{Try, Success, Failure}

enum Provider:
  case QuickNode
  case WatchData
  case Elrond

object Provider:

  given ConfigReader[Provider] = ConfigReader.fromString { str =>
    Try(Provider.valueOf(str)) match
      case Success(p) => Right(p)
      case Failure(e) => Left(ExceptionThrown(e))
  }

final case class Endpoint(
  chain:    Chain,
  uri:      Uri,
  apiKey:   String
) derives ConfigReader

object Endpoint:

  def select(provider: Provider, chain: Chain)(m: Map[Provider, Seq[Endpoint]]): Option[Endpoint] =
    m.get(provider).flatMap(_.find(_.chain == chain))
