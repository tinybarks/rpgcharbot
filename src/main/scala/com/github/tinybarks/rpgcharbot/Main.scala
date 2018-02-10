package com.github.tinybarks.rpgcharbot

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._
import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import com.vdurmont.emoji.EmojiParser
import fs2._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Main extends App with LazyLogging {
  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  val config = BotConfig()
  val instance = config.instance
  val token = config.token

  val templateEngine = TemplateEngine(config.wordLists, config.templateMap)
  def templateEngineWithName(name: String): TemplateEngine =
    templateEngine.copy(templateEngine.wordLists + ("_name" -> Seq(name)))

  val notifications: Stream[IO, Seq[Mention]] =
    Mastodon
      .getNotifications(instance, token)
      .map {
        case Left(err) => logger.error("Failed to get notifications:", err); Seq()
        case Right(res) => res
      }

  val sendStatus = Mastodon.sendStatus(instance, token, _: Option[Long], _: String)

  import Pipes._

  val program = for {
    scheduler <- Scheduler[IO](1)
    success <- scheduler.awakeEvery[IO](5.seconds)
      .flatMap(_ => notifications)
      .through(uniqueMentions)
      .through(inspect(m => logger.debug(s"Filtered mentions: $m")))
      .through(toA)
      .flatMap { m =>
        logger.info(s"Received mention: $m")
        val te = templateEngineWithName(m.displayName)
        val result = TemplateEngine.interpret(te, "final_message")
        logger.info(result.toString)
        result match {
          case Valid(reply) =>
            val replyWithEmoji = EmojiParser.parseToUnicode(reply)
            sendStatus(m.statusId.some, s"${m.username}\n$replyWithEmoji")
          case Invalid(errors) =>
            val message = "Failed to post update:\n" + errors.toList.mkString("\n")
            logger.error(message)
            Stream.emit(message)
        }
      }
  } yield ()

  logger.info(s"Starting to poll for notifications on ${config.instance}...")
  program.run.unsafeRunSync()
}
