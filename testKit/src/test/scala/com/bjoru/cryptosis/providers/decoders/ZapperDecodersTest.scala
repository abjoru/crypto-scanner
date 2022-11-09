package com.bjoru.cryptosis.providers.decoders

import cats.effect.IO
import cats.syntax.eq.*
import cats.syntax.traverse.*

import io.circe.*
import io.circe.parser.parse

import com.bjoru.cryptosis.types.*

import scala.io.Source

import munit.CatsEffectSuite

class ZapperDecodersTest extends CatsEffectSuite:

  import ZapperDecoders.{*, given}

  extension (r: ZapperResult)
    def find(sym: String): Option[PricedToken] =
      r.tokens.find(_.token.symbol eqv Symbol(sym))

    def findDapp(name: String): Option[Defi] =
      r.apps.find(_.name == name)

    def findFarm(name: String): Option[Defi.Farm] =
      r.findDapp(name).flatMap {
        case d: Defi.Farm => Some(d)
        case _            => None
      }

    def assertBalanceAndPrice(sym: String, balance: Balance, price: Price) = r.find(sym) match
      case Some(t) => 
        assert(t.price == price)
        assert(t.token.balance == balance)
      case None => fail(s"No such token: $sym")

    def assertFarmLQ(name: String, lq: (String, Balance)*) =
      r.findDapp(name) match 
        case Some(d: Defi.Farm) => lq.foreach {
          case (sym, bal) => d.liquidity.find(_.symbol eqv Symbol(sym)) match
            case Some(t) => assert(t.balance == bal)
            case None    => fail(s"No liquidity with symbol: $sym")
        }
        case other => fail(s"Not a farm: $other")

    def assertFarmCL(name: String, cl: (String, Balance)*) =
      r.findDapp(name) match 
        case Some(d: Defi.Farm) => cl.foreach {
          case (sym, bal) => d.claimable.find(_.symbol eqv Symbol(sym)) match
            case Some(t) => assert(t.balance == bal)
            case None    => fail(s"No clamiable with symbol: $sym")
        }
        case other => fail(s"Not a farm: $other")

  def load(filename: String): IO[Json] = 
    val src = Source.fromResource(s"com/bjoru/cryptosis/providers/decoders/$filename", getClass.getClassLoader)
    val doc = src.getLines.mkString("\n")
    IO.fromEither(parse(doc))

  test("parse avalanche balances") {
    for json <- load("balance-avalanche.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.tokens.size == 4)
        _ = res.assertBalanceAndPrice("bsgg", Balance(1385.4718925647153), Price(0.00361797))
        _ = res.assertBalanceAndPrice("wavax", Balance(0.06559706486576682), Price(18.19))
        _ = res.assertBalanceAndPrice("avax", Balance(2.1905622337480057), Price(18.12))
        _ = res.assertBalanceAndPrice("usdc", Balance(10.442305), Price(1.001))
    yield ()
  }

  test("parse bitcoin balances") {
    for json <- load("balance-bitcoin.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.tokens.size == 1)
        _ = res.assertBalanceAndPrice("btc", Balance(0.3197079), Price(20328))
    yield ()
  }

  test("parse bsc balances") {
    for json <- load("balance-bsc.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.tokens.size == 2)
        _ = res.assertBalanceAndPrice("bnb", Balance(0.10731225246207596), Price(335.9))
        _ = res.assertBalanceAndPrice("wbnb", Balance(0.000250217246338276), Price(336.2))
    yield ()
  }

  test("parse ethereum balances") {
    for json <- load("balance-ethereum.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.tokens.size == 5)
        _ = res.assertBalanceAndPrice("enj", Balance(144.51295444), Price(0.438725))
        _ = res.assertBalanceAndPrice("uos", Balance(931.7981), Price(0.302964))
        _ = res.assertBalanceAndPrice("bal", Balance(2.62949213), Price(6.68))
        _ = res.assertBalanceAndPrice("eth", Balance(0.8765871136581569), Price(1542.62))
        _ = res.assertBalanceAndPrice("efi", Balance(1030.4078735025755), Price(0.109865))
    yield ()
  }

  test("parse fantom balances") {
    for json <- load("balance-fantom.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.tokens.size == 3)
        _ = res.assertBalanceAndPrice("crv", Balance(1.047757395116652), Price(0.929466))
        _ = res.assertBalanceAndPrice("ftm", Balance(128.21482124507665), Price(0.263549))
        _ = res.assertBalanceAndPrice("wftm", Balance(0.06923021132091133), Price(0.26362))
    yield ()
  }

  test("parse polygon balances") {
    for json <- load("balance-polygon.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.tokens.size == 3)
        _ = res.assertBalanceAndPrice("usdc", Balance(47.423543), Price(1.001))
        _ = res.assertBalanceAndPrice("wmatic", Balance(0.015581155935975206), Price(1.036))
        _ = res.assertBalanceAndPrice("matic", Balance(377.98369743319154), Price(1.038))
    yield ()
  }

  test("parse ethereum balancer dapp") {
    for json <- load("balancer-ethereum.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.findFarm("balancer-v2").map(_.liquidity.isEmpty).getOrElse(false))
        _ = res.assertFarmCL("balancer-v2", ("bal", Balance(1.1609927)))
    yield ()
  }

  test("parse fantom curve dapp") {
    for json <- load("curve-ftm.json")
        res  <- IO.fromEither(json.as[ZapperResult])

        _ = assert(res.findFarm("curve").map(_.liquidity.isEmpty).getOrElse(false))
        _ = res.assertFarmCL("curve", ("crv", Balance(0.5377675653110732)), ("wftm", Balance(1.3750212616089945)))
    yield ()
  }
