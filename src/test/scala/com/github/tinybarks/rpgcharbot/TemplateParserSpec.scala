package com.github.tinybarks.rpgcharbot

import org.specs2.mutable._

import cats._
import cats.implicits._
import atto._
import Atto._
import cats.data.{NonEmptyList => NEL}
import TemplateParser._

object TemplateParserSpec extends Specification {
  "template parser should be able to" >> {
    "parse text" >> {
      TemplateParser("hello") should beRight(NEL.one[Token](Text("hello")))
    }

    "understand escapes" >> {
      TemplateParser("hello {world}") should beRight(NEL.of[Token](Text("hello "), Escape(None, "world")))
    }

    "understand escapes with functions without args" >> {
      TemplateParser("hello {pick:world}") should
        beRight(NEL.of[Token](Text("hello "), Escape(Some(Function("pick", List())), "world")))
    }

    "understand escapes with functions with one or more args" >> {
      TemplateParser("hello {pick(1):world}") should
        beRight(NEL.of[Token](Text("hello "), Escape(Some(Function("pick", List("1"))), "world")))

      TemplateParser("hello {pick(1,2):world}") should
        beRight(NEL.of[Token](Text("hello "), Escape(Some(Function("pick", List("1", "2"))), "world")))
    }
  }
}
