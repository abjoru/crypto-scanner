package com.bjoru.cryptosis.types

import cats.effect.IO

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptosis.*
import com.bjoru.cryptosis.instances.given

final case class TokenFilter(
  chain:  Chain,
  ignore: Seq[Address]
) derives ConfigReader

object TokenFilter:

  extension (f: TokenFilter)

    def filterTokens(tokens: Seq[Token]): Seq[Token] =
      tokens.filterNot { token =>
        token.contract.map(f.ignore.contains(_)).getOrElse(false)
      }

  def empty(chain: Chain) = TokenFilter(chain, Seq.empty)

  def loadTokenFilters(file: FilePath): IO[Seq[TokenFilter]] =
    loadYaml[Map[Chain, Seq[Address]]](file).map { rs =>
      rs.map(kv => TokenFilter(kv._1, kv._2)).toSeq
    }
