package com.bjoru.cryptosis.instances

import cats.syntax.traverse.given

import pureconfig.*
import pureconfig.syntax.{*, given}

import com.typesafe.config.ConfigValueFactory

import org.http4s.Uri

given ConfigReader[Uri] = ConfigReader.fromString { str =>
  Uri.fromString(str).left.map(e => error.CannotConvert(str, "org.http4s.Uri", e.getMessage))
}

given [T, U](using ConfigReader[T], ConfigReader[U]): ConfigReader[Map[T, U]] =
  ConfigReader.fromCursor { c =>
    c.asMap.flatMap { initMap =>
      val prepList = initMap.toList.traverse {
        case (k, v) =>
          for a <- ConfigValueFactory.fromAnyRef(k, "Map key").to[T]
              b <- v.to[U]
          yield a -> b
      }

      prepList.map(_.toMap)
    }
  }
