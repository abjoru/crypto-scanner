package com.bjoru.cryptosis.config

import java.io.*
import java.net.{URI, URL}
import java.nio.file.{Files, Path, Paths}
import java.util.Base64

import scala.collection.JavaConverters.*
import scala.util.Try
import scala.util.control.NonFatal

import com.typesafe.config.{ConfigOrigin, ConfigOriginFactory, ConfigValue, ConfigValueFactory}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor
import org.yaml.snakeyaml.error.{Mark, MarkedYAMLException, YAMLException}
import org.yaml.snakeyaml.nodes.Tag

import pureconfig.ConfigReader.Result
import pureconfig.error.*
import pureconfig.{ConfigObjectSource, ConfigSource}

final class YamlConfigSource private (
  getReader: () => Reader,
  uri: Option[URI] = None,
  onIOFailure: Option[Option[Throwable] => CannotRead] = None
) extends ConfigSource:

  private[this] def loader = new Yaml(new CustomConstructor())

  def value(): Result[ConfigValue] = usingReader { reader =>
    yamlObjToConfigValue(loader.load[AnyRef](reader))
  }

  def asObjectSource: ConfigObjectSource =
    ConfigObjectSource(fluentCursor().asObjectCursor.map(_.objValue.toConfig))

  def multiDoc: ConfigSource = new ConfigSource {
    def value(): Result[ConfigValue] = usingReader { reader =>
      loader.loadAll(reader)
        .asScala
        .map(yamlObjToConfigValue)
        .foldRight(Right(Nil): Result[List[ConfigValue]])(Result.zipWith(_, _)(_ :: _))
        .map(cvs => ConfigValueFactory.fromIterable(cvs.asJava))
    }
  }

  private[this] class CustomConstructor extends SafeConstructor:
    yamlConstructors.put(Tag.TIMESTAMP, new ConstructYamlStr())

  private[this] def yamlObjToConfigValue(obj: AnyRef): Result[ConfigValue] =
    def aux(obj: AnyRef): Result[AnyRef] = obj match
      case m: java.util.Map[AnyRef @unchecked, AnyRef @unchecked] =>
        val entries: Iterable[Result[(String, AnyRef)]] = m.asScala.map {
          case (k: String, v) => aux(v).map((v: AnyRef) => k -> v)
          case (k, _)         => Left(ConfigReaderFailures(NonStringKeyFound(k.toString, k.getClass.getSimpleName)))
        }
        Result.sequence(entries).map(_.toMap.asJava)

      case xs: java.util.List[AnyRef @unchecked] =>
        Result.sequence(xs.asScala.map(aux)).map(_.toList.asJava)

      case s: java.util.Set[AnyRef @unchecked] =>
        Result.sequence(s.asScala.map(aux)).map(_.toSet.asJava)

      case _: java.lang.Integer | _: java.lang.Long | _: java.lang.Double | _: java.lang.String |
            _: java.lang.Boolean =>
          Right(obj) // these types are supported directly by `ConfigValueFactory.fromAnyRef`

      case _: java.math.BigInteger =>
        Right(obj.toString)

      case ba: Array[Byte] =>
        Right(Base64.getEncoder.encodeToString(ba))

      case null =>
        Right(null)

      case _ => // this shouldn't happen
        Left(ConfigReaderFailures(UnsupportedYamlType(obj.toString, obj.getClass.getSimpleName)))

    aux(obj).map(ConfigValueFactory.fromAnyRef)

  private[this] def usingReader[A](f: Reader => Result[A]): Result[A] = try {
    val reader = getReader()
    try f(reader)
    finally Try(reader.close())
  } catch {
    case e: IOException if onIOFailure.nonEmpty =>
      Result.fail(onIOFailure.get(Some(e)))
    case e: MarkedYAMLException =>
      Result.fail(CannotParse(e.getProblem, uri.map { uri => toConfigOrigin(uri.toURL, e.getProblemMark) }))
    case e: YAMLException =>
      Result.fail(CannotParse(e.getMessage, None))
    case NonFatal(e) =>
      Result.fail(ThrowableFailure(e, None))
  }
  
  private[this] def toConfigOrigin(path: URL, mark: Mark): ConfigOrigin = {
    ConfigOriginFactory.newURL(path).withLineNumber(mark.getLine + 1)
  }

object YamlConfigSource:

  def file(path: String) = new YamlConfigSource(
    () => new FileReader(path),
    uri = Some(new File(path).toURI),
    onIOFailure = Some(CannotReadFile(Paths.get(path), _))
  )

  def file(path: Path) = new YamlConfigSource(
    () => Files.newBufferedReader(path),
    uri = Some(path.toUri),
    onIOFailure = Some(CannotReadFile(path, _))
  )

  def file(file: File) = new YamlConfigSource(
    () => new FileReader(file),
    uri = Some(file.toURI),
    onIOFailure = Some(CannotReadFile(file.toPath, _))
  )

  def string(confStr: String) = new YamlConfigSource(() => new StringReader(confStr))
