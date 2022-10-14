package com.bjoru.cscanner.api

import cats.effect.IO

import io.circe.*
import io.circe.syntax.{*, given}

import org.http4s.Uri
import org.http4s.dsl.io.*
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.circe.*
import org.http4s.circe.CirceEntityDecoder.given

import com.bjoru.cscanner.*
import com.bjoru.cscanner.types.*
import com.bjoru.cscanner.utils.*
import com.bjoru.cscanner.config.findEndpoint
import Quantity.decodeQuantity

import scala.util.{Try, Success, Failure}

import java.nio.file.Path

class ZapperApi(cfgDir: Path, chain: Chain):

  enum AppId(val id: String):
    case OneInch extends AppId("1inch")
    case AaveAmm extends AppId("aave-amm")
    case AaveV1 extends AppId("aave-v1")
    case AaveV2 extends AppId("aave-v2")
    case AaveV3 extends AppId("aave-v3")
    case Abracadabra extends AppId("abracadabra")
    case Adamant extends AppId("adamant")
    case Aelin extends AppId("aelin")
    case Agave extends AppId("agave")
    case AirSwap extends AppId("airswap")
    case AlchemixV2 extends AppId("alchemix-v2")
    case Alchemix extends AppId("alchemix")
    case Alkemi extends AppId("alkemi")
    case ApeSwap extends AppId("apeswap")
    case AutoFarm extends AppId("autofarm")
    case BalancerV1 extends AppId("balancer-v1")
    case BalancerV2 extends AppId("balancer-v2")
    case Beefy extends AppId("beefy")
    case BeethovenX extends AppId("beethoven-x")
    case BiSwap extends AppId("biswap")
    case Compound extends AppId("compound")
    case Convex extends AppId("convex")
    case Curve extends AppId("curve")
    case DefiKingdoms extends AppId("defi-kingdoms")
    case Grim extends AppId("grim")
    case Gro extends AppId("gro")
    case Harvest extends AppId("harvest")
    case HectorDao extends AppId("hector-network")
    case HomoraV2 extends AppId("homora-v2")
    case KlimaDao extends AppId("klima")
    case Maker extends AppId("maker")
    case PancakeSwap extends AppId("pancakeswap")
    case Pangolin extends AppId("pangolin")
    case Popsicle extends AppId("popsicle")
    case QuickSwap extends AppId("quickswap")
    case Reaper extends AppId("reaper")
    case ScreamV2 extends AppId("scream-v2")
    case Scream extends AppId("scream")
    case SpiritSwap extends AppId("spiritswap")
    case SpookySwap extends AppId("spookyswap")
    case SushiSwap extends AppId("sushiswap")
    case Symphony extends AppId("symphony")
    case TrakerJoeBanker extends AppId("trader-joe-banker")
    case TrakerJoe extends AppId("trader-joe")
    case UniSwapV1 extends AppId("uniswap-v1")
    case UniSwapV2 extends AppId("uniswap-v2")
    case UniSwapV3 extends AppId("uniswap-v3")
    case Wonderland extends AppId("wonderland")
    case Yearn extends AppId("yearn")

    def supportedBalances(wallets: Set[Wallet])(client: Client[IO]): IO[Json] =
      val addrs = wallets.map(_.address.stringValue).mkString(",")

      for u <- IO.fromEither(Uri.fromString(s"https://api.zapper.fi/v2/apps/balances/supported?addresses[]=$addrs"))
          j <- client.expect(u)(jsonOf[IO, Json])
      yield j

    def appBalances(app: AppId, wallets: Set[Wallet])(client: Client[IO]): IO[Json] =
      val addrs = wallets.map(_.address.stringValue).mkString(",")

      for u <- IO.fromEither(Uri.fromString(s"https://api.zapper.fi/v2/apps/${app.id}/balances?addresses[]=$addrs"))
          j <- client.expect(u)(jsonOf[IO, Json])
      yield j

