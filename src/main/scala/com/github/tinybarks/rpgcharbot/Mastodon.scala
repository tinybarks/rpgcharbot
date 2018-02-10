package com.github.tinybarks.rpgcharbot

import cats.effect._
import com.google.gson.Gson
import com.sys1yagi.mastodon4j.MastodonClient
import com.sys1yagi.mastodon4j.api.method.{Notifications, Statuses}
import okhttp3.OkHttpClient
import fs2._
import cats._
import cats.implicits._
import com.sys1yagi.mastodon4j.api.entity.Status
import com.sys1yagi.mastodon4j.api.entity.Status.Visibility

import scala.collection.JavaConverters._

object Mastodon {
  /**
    * Returns a Mastodon client for the account on `instance` (e.g. `"mastodon.social"`) identified by the `token`.
    */
  def makeClient(instance: String, token: String): MastodonClient =
    new MastodonClient.Builder(instance, new OkHttpClient.Builder(), new Gson())
      .accessToken(token)
      .build()

  /**
    * Gets notifications from the Mastodon account on `instance` (e.g. `"mastodon.social"`) identified by the
    * `token` and returns the first page of [[Mention]]s from the Mastodon API.
    *
    * @note Blocking.
    * @throws RuntimeException Some random exception that no one cares about on failure.
    */
  def getNotificationsBlocking(instance: String, token: String): Seq[Mention] = {
    val client = makeClient(instance, token)
    val notifications = new Notifications(client)

    notifications.getNotifications.execute().getPart
      .asScala
      .filter(_.getType == "mention")
      .map(n => Mention(
        n.getId,
        Option(n.getStatus).map(_.getId).getOrElse(0L),
        Option(n.getAccount).map { acc =>
          if (acc.getDisplayName.trim.isEmpty) {
            acc.getUserName
          } else {
            acc.getDisplayName
          }
        }.getOrElse(""),
        Option(n.getAccount).map("@" + _.getAcct).getOrElse(""),
        Option(n.getStatus).map(_.getContent).map(Util.stripTags).getOrElse(""),
      ))
  }

  /**
    * Gets notifications from the Mastodon account on `instance` (e.g. `"mastodon.social"`) identified by the
    * `token` and returns the first page of [[Mention]]s from the Mastodon API.
    */
  def getNotifications(instance: String, token: String): Stream[IO, Either[Throwable, Seq[Mention]]] =
    Stream.attemptEval(IO {
      getNotificationsBlocking(instance, token)
    })

  /**
    * Sends a status from the Mastodon account on `instance` (e.g. `"mastodon.social"`) identified by the
    * `token` with the given text, replying to the given status.
    *
    * @note Blocking.
    * @throws RuntimeException Some random exception that no one cares about on failure.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def sendStatusBlocking(instance: String, token: String, replyToStatusId: Option[Long], text: String): Status = {
    val client = makeClient(instance, token)
    val statuses = new Statuses(client)
    val num: Long = replyToStatusId.getOrElse(0L)

    statuses.postStatus(
      text,
      num,
      null, // media ids
      false, // sensitive
      null, // spoiler text
      Visibility.Direct).execute()
  }

  /**
    * Sends a status from the Mastodon account on `instance` (e.g. `"mastodon.social"`) identified by the
    * `token` with the given text, replying to the given status.
    *
    * @note Blocking.
    * @throws RuntimeException Some random exception that no one cares about on failure.
    */
  def sendStatus(instance: String, token: String, replyToStatusId: Option[Long], text: String): Stream[IO, Either[Throwable, Status]] =
    Stream.attemptEval(IO {
      sendStatusBlocking(instance, token, replyToStatusId, text)
    })
}