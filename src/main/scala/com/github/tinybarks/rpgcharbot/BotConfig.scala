package com.github.tinybarks.rpgcharbot

import java.io.File
import com.typesafe.config.ConfigFactory
import pureconfig.error.ConfigReaderFailures
import pureconfig.{ConfigReader, Derivation}
import scala.util.Try

import TemplateEngine.{TemplateMap, WordLists}

/**
  * Holds the bot's configuration.
  */
case class BotConfig(instance: String, token: String, discordToken: Option[String], wordLists: WordLists, templateMap: TemplateMap)

/**
  * Holds the bot's configuration.
  */
@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.EitherProjectionPartial"))
object BotConfig {

  // wrapper because pureconfig does naughty things we don't care about
  private def load[T](path: String)(implicit reader: Derivation[ConfigReader[T]]): Either[ConfigReaderFailures, T] = {
    val config = Try(ConfigFactory.parseFile(new File(path))).toEither
      .left.map(e => throw new RuntimeException(s"Failed to load config file '$path'", e))
      .right.get

    pureconfig.loadConfig[T](config)
  }

  private lazy val wordLists: WordLists =
    load[WordLists]("conf/words.conf")
      .left.map(e => throw new RuntimeException(s"Failed to parse word list: ${e.toList.mkString("\n")}"))
      .right.get

  private lazy val templateMap: TemplateMap =
    load[Map[String, String]]("conf/templates.conf")
      .left.map(e => throw new RuntimeException(s"Failed to parse templates file: ${e.toList.mkString("\n")}"))
      .right.get
      .mapValues(TemplateEngine.parse)
      .map {
        case (name, Left(err)) => throw new RuntimeException(s"Error in template '$name': $err")
        case (name, Right(t)) => (name, t)
      }

  private case class ApplicationConf(instance: String, token: String, discordToken: Option[String])

  private lazy val config: ApplicationConf =
    load[ApplicationConf]("conf/application.conf")
      .left.map(e => throw new RuntimeException(s"Failed to parse config: ${e.toList.mkString("\n")}"))
      .right.get


  /**
    * Retrieves the bot's configuration.
    */
  def apply(): BotConfig = BotConfig(config.instance, config.token, config.discordToken, wordLists, templateMap)
}
