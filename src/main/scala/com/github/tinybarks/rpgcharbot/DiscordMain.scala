package com.github.tinybarks.rpgcharbot

import com.typesafe.scalalogging.LazyLogging
import net.dv8tion.jda.core.AccountType
import net.dv8tion.jda.core.JDA
import net.dv8tion.jda.core.JDABuilder
import net.dv8tion.jda.core.events.{Event, ReadyEvent}
import net.dv8tion.jda.core.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.core.hooks.{EventListener, ListenerAdapter}

import scala.collection.JavaConverters._

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.TraversableOps"))
object DiscordMain extends App with LazyLogging {
  val config = BotConfig()

  val templateEngine = TemplateEngine(config.wordLists, config.templateMap)
  def templateEngineWithName(name: String): TemplateEngine =
    templateEngine.copy(templateEngine.wordLists + ("_name" -> Seq(name)))

  var emojis: Map[String, String] = Map()

  val listener = new ListenerAdapter {
    override def onReady(event: ReadyEvent): Unit = {
      emojis = event.getJDA.getEmotes.asScala
        .filter(_.getGuild.getId == "381523766074933248")
        .map { e => (":" + e.getName + ":", e.getAsMention) }
        .toMap

      println(emojis)
    }

    override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit = {
      val isMentioned = event.getMessage.getMentionedUsers.contains(event.getJDA.getSelfUser)
      logger.info(s"${event.getMessage} $isMentioned")
      val chan = event.getMessage.getChannel
      if (isMentioned && chan.getId == "382923477310767104") {
        val te = templateEngineWithName(event.getAuthor.getName)
        val templ = TemplateEngine.interpret(te, "final_message")
        logger.info(templ.toString)
        logger.info(event.getGuild.getId)
        templ.map { result =>
          val resultWithEmojis = emojis.foldLeft(result) { case (r, (name, literal)) => r.replace(name, literal) }
          chan.sendMessage(resultWithEmojis).complete(true)
        }
      }
    }
  }

  config.discordToken.map { token =>
    new JDABuilder(AccountType.BOT).addEventListener(listener).setToken(token).buildBlocking
  }.orElse {
    throw new RuntimeException("No 'discord-token' set in the config file.")
  }
}
