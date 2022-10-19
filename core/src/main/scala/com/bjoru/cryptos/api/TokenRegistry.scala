package com.bjoru.cryptos.api

import cats.effect.IO

import com.bjoru.cryptos.types.*
import com.bjoru.cryptos.utils.{*, given}
import com.bjoru.cryptos.api.types.GToken

import java.nio.file.Path

class TokenRegistry(cfgDir: Path):

  lazy val registry: IO[Seq[GToken]] = 
    GToken.loadGTokens(cfgDir </> "tokens.yaml")

  def findToken(chain: Chain, contract: Address): IO[Option[GToken]] =
    registry.map { tokens =>
      tokens.find(_.platforms.get(chain).map(_ == contract).getOrElse(false))
    }

  def findTokens(chain: Chain, contracts: Address*): IO[Seq[GToken]] =
    registry.map { tokens =>
      val chainTokens = tokens.filter(_.platforms.contains(chain))
      chainTokens.filter(t => contracts.contains(t.platforms(chain)))
    }

  def findBySymbol(symbol: Symbol): IO[Seq[GToken]] =
    registry.map(_.filter(_.symbol == symbol))
