package com.bjoru.cryptos.instances

import cats.Show
import cats.syntax.traverse.given

import io.circe.Decoder

import pureconfig.*
import pureconfig.syntax.{*, given}
import com.typesafe.config.ConfigValueFactory

import org.http4s.Uri

import java.text.NumberFormat

val numberFormatter = NumberFormat.getInstance
val currencyFormatter = NumberFormat.getCurrencyInstance
val percentFormatter = NumberFormat.getPercentInstance

//////////
// Show //
//////////

//given Show[BigDecimal] = ???

//////////////
// Decoders //
//////////////

given Decoder[Uri] = Decoder.decodeString.emapTry(s => Uri.fromString(s).toTry)

////////////////////
// Config Readers //
////////////////////

given ConfigReader[Uri] = ConfigReader.fromString { str =>
  Uri.fromString(str).left.map(e => error.CannotConvert(str, "Uri", e.getMessage))
}

given [T, U](using ConfigReader[T], ConfigReader[U]): ConfigReader[Map[T, U]] =
  ConfigReader.fromCursor { c =>
    c.asMap.flatMap { initMap =>
      val prepList = initMap.toList.traverse {
        case (key, value) =>
          for a <- ConfigValueFactory.fromAnyRef(key, "map keys").to[T]
              b <- value.to[U]
          yield a -> b
      }

      prepList.map(_.toMap)
    }
  }
