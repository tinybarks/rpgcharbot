package com.github.tinybarks.rpgcharbot

import cats.data.{NonEmptyList, ValidatedNel}
import cats._
import cats.implicits._

import scala.util.{Failure, Random, Success, Try}

case class TemplateEngine(wordLists: TemplateEngine.WordLists, templates: TemplateEngine.TemplateMap)

object TemplateEngine {
  type Template = NonEmptyList[Token]
  type WordLists = Map[String, Seq[String]]
  type TemplateMap = Map[String, Template]
  type InterpretResult = ValidatedNel[String, String]

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
  def parse(text: String): Either[String, Template] = TemplateParser(text)

  /**
    * Interprets the template in the given template engine.
    * @return Left: Template interpretation error message, Right: The interpreted template.
    */
  def interpret(te: TemplateEngine, templateName: String): InterpretResult =
    te.templates.get(templateName)
      .toValidNel(s"Couldn't find template '$templateName'!")
      .andThen(t => interpret(te, templateName, t))

  def interpret(te: TemplateEngine, templateName: String, template: Template): InterpretResult =
    template.map {
      case e: Escape => interpretEscape(te, templateName, e)
      case Text(t) => t.valid
    }.reduce

  def interpretEscape(te: TemplateEngine, templateName: String, escape: Escape): InterpretResult =
    escape match {
      case Escape(Some(fn), rest) => interpretFunction(te, templateName, fn, rest)
      case Escape(None, rest) => interpretRest(te, templateName, rest)
    }

  // TODO make random implicit arg
  def interpretFunction(te: TemplateEngine, templateName: String, fn: Function, rest: String): InterpretResult =
    fn match {
      case Function("pick", List()) =>
        interpretRest(te, templateName, Random.shuffle(rest.split(',').seq).headOption.getOrElse("???"))

      case f@Function(_, _) =>
        s"[$f]".validNel
      // case Function(name, _) => s"Unknown function '$name' encountered in template '$templateName'!".invalidNel
    }

  def interpretRest(te: TemplateEngine, templateName: String, rest: String): InterpretResult =
    rest match {
      case wordList if wordList.startsWith("$") =>
        interpretWordList(te, templateName, wordList.tail)

      case template if template.startsWith("#") =>
        Try(interpret(te, template.tail)) match {
          case Success(res) => res
          case Failure(_: StackOverflowError) => s"There is a circular reference between $templateName and $template!".invalidNel
          case Failure(e: Throwable) => s"Unexpected exception: $e".invalidNel
        }
      case word => word.validNel
    }

  // TODO make random implicit arg
  def interpretWordList(te: TemplateEngine, templateName: String, wordListName: String): InterpretResult =
    te.wordLists.get(wordListName)
      .toValidNel(s"Couldn't find wordlist '$wordListName' referenced in '$templateName'.")
      .andThen { wl =>
        val entry = Random.shuffle(wl).headOption.getOrElse("[empty wordlist!]")

        parse(entry)
          .map(t => interpret(te, s"wordlist '$wordListName' ($entry)", t)) match {
          case Left(parseFail) => s"Failed to parse entry '$entry' in wordlist '$wordListName': $parseFail".invalidNel
          case Right(interpretedWord) => interpretedWord
        }
      }


  // val testTemplate = parse("Hello {$names}! I wish you a {maybe(20%):$superlative} {pick:nice,great} day! {#salutation}").right.get
  // val testTemplate2 = parse("(This text in brackets from another template!)").right.get
  // val te = TemplateEngine(
  //   wordLists = Map(
  //     "names" -> Seq("Kate", "Gapow", "VoxSomniator", "caff", "Gyro", "trashbyte"),
  //     "test" -> Seq("hello", "world"),
  //     "cool_exclamation" -> Seq("Cool!", "Awesome!", "Epic!", "Splendid!")
  //   ),
  //   templates = Map(
  //     "testTemplate" -> testTemplate,
  //     "salutation" -> testTemplate2,
  //   )
  // )
  //
  // Seq(
  //   interpret(te, "testTemplate"),
  //   interpret(te, "testTemplate"),
  //   interpret(te, "testTemplate"),
  //   interpret(te, "testTemplate")
  // ).foreach(println)
}