package com.github.tinybarks.rpgcharbot

import cats._
import cats.implicits._
import atto._
import Atto._
import cats.data.NonEmptyList

/**
  * A template parser token.
  */
sealed trait Token

/**
  * Represents a function with an optional list of arguments.
  */
case class Function(name: String, parameters: List[String])

/**
  * Represents an escape code that may contain an optional modifier.
  */
case class Escape(fn: Option[Function], text: String) extends Token

/**
  * Represents text (i.e. everything that's not an escape code).
  */
case class Text(text: String) extends Token


object TemplateParser {
  val notEscapeEnd: Parser[String] = stringOf1(notChar('}'))
  val functionName: Parser[String] = stringOf1(noneOf("}(:"))
  val parameterText: Parser[String] = stringOf1(noneOf("},)"))
  val parameterList: Parser[List[String]] = parameterText.sepBy(char(',')).map(_.map(_.trim))

  val functionWithParameters: Parser[Function] =
    (functionName ~ char('(') ~ parameterList <~ char(')'))
      .map { case ((name, _), params) => Function(name, params) }

  val functionWithoutParameters: Parser[Function] =
    functionName
      .map { Function(_, List()) }

  val function: Parser[Function] = functionWithParameters | functionWithoutParameters

  val escapeFn: Parser[Escape] =
    (function ~ char(':') ~ notEscapeEnd).map { case ((f, _), t) => Escape(f.some, t) }

  val escape1: Parser[Escape] = notEscapeEnd.map(s => Escape(None, s))

  val escape: Parser[Escape] = char('{') ~> (escapeFn | escape1) <~ char('}')

  val text: Parser[Token] = stringOf1(notChar('{')).map(Text)

  val parser: Parser[NonEmptyList[Token]] = many1(escape | text)

  /**
    * Parses a template for text generation. The template supports escape codes with optional function invocations:
    *
    * ```
    * Hello {$names}! I wish you a {maybe(20%):$superlative} {pick:nice,great} day!
    *
    * {!salutation}
    * ```
    *
    * @return Either a message describing a parse error, or a list of tokens.
    */
  def apply(text: String): Either[String, NonEmptyList[Token]] = parser.parseOnly(text).either

  // println(parser.parseOnly("aaa {b} aaa aaaa{test:arg} ds {test(1):what} {test(1,2):what}").either)
  // println("\nfunction")
  // println(function.parseOnly("hello").either)
  // println(function.parseOnly("hello(1)").either)
  // println(function.parseOnly("hello(1, 2)").either)
  // println("\nfunctionWithParameters")
  // println(functionWithParameters.parseOnly("hello").either)
  // println(functionWithParameters.parseOnly("hello(1)").either)
  // println(functionWithParameters.parseOnly("hello(1, 2)").either)
  // println("\nparameterList")
  // println(parameterList.parseOnly("a").either)
  // println(parameterList.parseOnly("a,   b").either)
}
