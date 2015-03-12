package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite

/**
 * Created by montag on 12.03.15.
 */
class RegexpPatternTest extends FunSuite {
    val allNumbers = new RegexpPattern((0 to 9).map(i => ElementClass(1)))
    val allLetters = new RegexpPattern((0 to 9).map(i => ElementClass(3)))
    val twoWords = new RegexpPattern(Seq(ElementClass(3), ElementClass(3), ElementClass(3), ElementClass(2), ElementClass(3), ElementClass(3)))

    test("test simple generation of regexps") {
        assertResult("\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d")(allNumbers.stringRepresentation)
        assertResult("\\w\\w\\w\\w\\w\\w\\w\\w\\w\\w")(allLetters.stringRepresentation)
        assertResult("\\w\\w\\w\\s\\w\\w")(twoWords.stringRepresentation)
    }

    test("test pattern matching") {
        assertResult(List("1234567890", "0000000000"))(allNumbers.findMatchAll("asdas1234567890akjakasd0000000000"))
        assertResult(List("1234567890", "0000000000"))(allNumbers.findMatchAll("asdas12345678900000000000"))
        assertResult(List("aab ba", "foo ba"))(twoWords.findMatchAll("aab bafoo bar"))
    }

    test("test pack") {
        assertResult("\\d+")(allNumbers.compressedStringRepresentation)
        assertResult("\\w+")(allLetters.compressedStringRepresentation)
        assertResult("\\w+\\s\\w+")(twoWords.compressedStringRepresentation)
    }
}
