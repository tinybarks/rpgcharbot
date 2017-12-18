//noinspection TypeAnnotation
package com.github.tinybarks.rpgcharbot

import org.specs2.matcher.{BeOneOf, MatchResult}
import org.specs2.mutable._

object TemplateEngineSpec extends Specification {
implicit class TemplateEngineWithAdd(engine: TemplateEngine) {
    def addTemplate(templateName: String, template: String): TemplateEngine = {
      // will blow up if something goes wrong
      val parsed = TemplateEngine.parse(template).right.get
      engine.copy(templates = engine.templates + (templateName -> parsed))
    }
  }

  implicit class TemplateTestString(template: String) {
    val interpreted: String =
      TemplateEngine
        .interpret(te.addTemplate("test", template), "test")
        .bimap(e => throw new RuntimeException(s"Failed to evaluate template: $e"), s => s)
        .getOrElse(throw new RuntimeException("Shouldn't happen"))

    def shouldInterpretTo(what: String) =
      interpreted should_== what

    def shouldInterpretToOneOf(what: Seq[String]) =
      interpreted must BeOneOf(what)

  }

  implicit class TemplateTestMap(templates: Map[String, String]) {
    val interpreted: String = {
      val engine = templates.foldLeft(te) { case (e, (name, template)) => e.addTemplate(name, template) }

      TemplateEngine
        .interpret(engine, "test")
        .bimap(e => throw new RuntimeException(s"Failed to parse template: $e"), s => s)
        .getOrElse(throw new RuntimeException("Shouldn't happen"))
    }

    def shouldInterpretTo(what: String) =
      interpreted should_== what

    def shouldInterpretToOneOf(what: Seq[String]) =
      interpreted must BeOneOf(what)
  }

  val te = TemplateEngine(
    wordLists = Map(
      "names" -> Seq("Kate", "Gapow", "VoxSomniator", "caff", "Gyro", "trashbyte"),
      "one" -> Seq("uno"),
      "two" -> Seq("dos"),
      "cool" -> Seq("cool"),
    ),

    templates = Map()
  )

  "TemplateEngine" should {
    "parse basic templates" >> {
      "Hello, world!" shouldInterpretTo "Hello, world!"
    }

    "let you insert a random word from a wordlist" >> {
      "Hello, {$names}!" shouldInterpretToOneOf Seq(
        "Hello, Gapow!",
        "Hello, VoxSomniator!",
        "Hello, caff!",
        "Hello, Gyro!",
        "Hello, trashbyte!",
        "Hello, Kate!"
      )
    }

    "support the {pick} verb on words" >> {
      "super{pick:duper,whooper}!" shouldInterpretToOneOf Seq("superwhooper!", "superduper!")
    }

    "support the {pick} verb on wordlists" >> {
      "super{pick:$one,$two}!" shouldInterpretToOneOf Seq("superuno!", "superdos!")
    }

    "support the {pick} verb on templates" >> {
      Map("hello" -> "hello", "bark" -> "bark", "test" -> "{pick:#hello,#bark}") shouldInterpretToOneOf Seq("hello", "bark")
    }

    "support the {pick} verb on mixed things" >> {
      "super{pick:$one,hello}!" shouldInterpretToOneOf Seq("superhello!", "superuno!")
    }

    "support the {even-pick} verb" >> {
      "super{even-pick:$one,$two}!" shouldInterpretToOneOf Seq("superuno!", "superdos!")
    }

    "support the {maybe} verb" >> {
      "{maybe(50%):not}good" shouldInterpretToOneOf Seq("notgood", "good")
      "{maybe(50%):$one}{$two}" shouldInterpretToOneOf Seq("unodos", "dos")
    }

    "support {capitalize} to capitalize the first letter" >> {
      "{capitalize:whoa}!" shouldInterpretTo "Whoa!"
      "{capitalize:$one}!" shouldInterpretTo "Uno!"
    }

    "support nested templates" >> {
      Map("cool" -> "{capitalize:$cool}!", "test" -> "{pick:#cool,$one}?") shouldInterpretToOneOf Seq("Cool!?", "uno?")
      "{capitalize:whoa}!" shouldInterpretTo "Whoa!"
      "{capitalize:$one}!" shouldInterpretTo "Uno!"
    }
  }
}
