package com.bjoru.cryptosis.types

import cats.effect.IO

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

import com.bjoru.cryptosis.*

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
