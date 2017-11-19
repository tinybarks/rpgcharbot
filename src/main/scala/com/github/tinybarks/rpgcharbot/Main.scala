package com.github.tinybarks.rpgcharbot

import cats._
import cats.implicits._
import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
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
        sendStatus(m.statusId.some, s"${m.username} hello ${m.displayName}, you said: ${m.content}")
      }
    _ = success.left.map(e => logger.error("Failed to post update: ", e))
  } yield ()

  logger.info("Starting to poll for notifications...")
  program.run.unsafeRunSync()
}
