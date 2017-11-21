package com.github.tinybarks.rpgcharbot

import com.github.tinybarks.rpgcharbot.Util.Chance
import org.specs2.mutable.Specification
import org.specs2.specification.AllExpectations

object UtilSpec extends Specification with AllExpectations {
  "stripTags should remove HTML" >> {
    Util.stripTags("""<a href="https://example.com">Interesting...</a>""") === "Interesting..."
    Util.stripTags("""<a>Int<b>eres</b>ting...</a>""") === "Interesting..."
    Util.stripTags("""<a>Broken H</b>TML""") === "Broken HTML"
  }

  "Chance" >> {
    "should match percentages properly" >> {
      Chance.unapply("0%") should beSome(beCloseTo(0d, 0.01d))
      Chance.unapply("25%") should beSome(beCloseTo(0.25d, 0.01d))
      Chance.unapply("77.6%") should beSome(beCloseTo(0.776d, 0.01d))
      Chance.unapply("100%") should beSome(beCloseTo(1d, 0.01d))
    }
  }
}
