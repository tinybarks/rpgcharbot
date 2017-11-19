package com.github.tinybarks.rpgcharbot

import org.jsoup.Jsoup

/**
  * Utility functions.
  */
object Util {
  /**
    * Strips HTML tags from the given string using Jsoup.
    */
  def stripTags(html: String): String =
    Jsoup.parse(html).text()
}
