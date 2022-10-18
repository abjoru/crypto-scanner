package com.bjoru.cryptos.types

import cats.effect.IO

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptos.config.*
import com.bjoru.cryptos.instances.given

import java.nio.file.Path

final case class TokenFilter(
  chain: Chain,
  ignore: Seq[Address]
) derives ConfigReader

object TokenFilter:

  extension (f: TokenFilter)

    def filterTokens(tokens: Seq[Token]): Seq[Token] =
      tokens.filterNot { t =>
        t.contract match
          case Some(c) => f.ignore.contains(c)
          case None    => false
      }

  def loadFilters(path: Path): IO[Seq[TokenFilter]] =
    loadYamlFile[Map[Chain, Seq[Address]]](path).map { rs =>
      rs.map(kv => TokenFilter(kv._1, kv._2)).toSeq
    }
