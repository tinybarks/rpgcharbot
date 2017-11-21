package com.github.tinybarks.rpgcharbot

import org.jsoup.Jsoup

import scala.util.Try
import scala.util.matching.Regex

/**
  * Utility functions.
  */
object Util {
  /**
    * Strips HTML tags from the given string using Jsoup.
    */
  def stripTags(html: String): String =
    Jsoup.parse(html).text()

  /**
    * A matcher that matches percentages ("0%" to "100%") and returns a double from 0 to 1 expressing chance.
    */
  //noinspection TypeAnnotation
  object Chance {
    private val ChanceRegex: Regex = """^(1?\d?\d(?:\.\d+)?)%$""".r

    def unapply(arg: String): Option[Double] =
      ChanceRegex
        .findFirstMatchIn(arg)
        .flatMap(nStr => Try(nStr.group(1).toDouble).toOption)
        .map(n => Math.min(Math.max(0d, n / 100d), 1d))
  }
}
