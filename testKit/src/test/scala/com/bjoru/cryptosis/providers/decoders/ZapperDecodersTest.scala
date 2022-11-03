package com.bjoru.cryptosis.providers.decoders

import cats.effect.IO
import cats.syntax.traverse.given

import io.circe.*
import io.circe.parser.parse

import scala.io.Source

import munit.CatsEffectSuite

class ZapperDecodersTest extends CatsEffectSuite:

  import ZapperDecoders.{*, given}

  val src = Source.fromResource("com/bjoru/cryptosis/providers/decoders/resp-new-bal.json", getClass.getClassLoader)

  lazy val lines = src.getLines.toSeq.filter {
    case line if line.contains("data: {}") => false
    case line if line.contains("data:")    => true
    case _                                 => false
  }

  test("Parse zapper response") {
    for items <- IO.fromEither(lines.traverse(l => parse(l.dropWhile(_ != '{'))))
        data  <- IO.fromEither(items.traverse(_.as[ZapperResult]))
        _     <- IO(reduce(data).foreach(println))
    yield assert(true)
  }

  private def reduce(data: Seq[ZapperResult]): Seq[ZapperResult] =
    data.groupBy(_.chain).foldLeft(Seq.empty[ZapperResult]) {
      case (acc, (_, xs)) => acc :+ xs.reduce {
        case (a, b) => a.copy(
            tokens = a.tokens ++ b.tokens,
            apps = a.apps ++ b.apps
          )
      }
    }
