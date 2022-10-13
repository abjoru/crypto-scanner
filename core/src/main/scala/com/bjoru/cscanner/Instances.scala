package com.bjoru.cscanner

import cats.syntax.traverse.given

import pureconfig.*
import pureconfig.syntax.{*, given}
import com.typesafe.config.ConfigValueFactory

import org.http4s.Uri

given ConfigReader[Uri] = ConfigReader.fromString { str => 
  Uri.fromString(str).left.map(e => error.CannotConvert(str, "Uri", e.getMessage)) 
}

given [T, U](using ConfigReader[T], ConfigReader[U]): ConfigReader[Map[T, Seq[U]]] = 
  ConfigReader.fromCursor { c =>
    c.asMap.flatMap { initMap =>
      val prepList = initMap.toList.traverse {
        case (key, value) => 
          for a <- ConfigValueFactory.fromAnyRef(key, "map keys").to[T]
              b <- value.to[Seq[U]]
          yield a -> b
      }

      prepList.map(_.toMap)
    }
  }