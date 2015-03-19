package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite

/**
 * Created by montag on 19.03.15.
 */
class PatternGeneratorTest extends FunSuite {
    val patternGenerator = new PatternGenerator {
        override protected def mine(facts: Seq[String], otherLines: Seq[String]): List[Pattern] = Nil
    }

    val line = "kdlfksldkfsdf<:fact>987398371243</:fact>;kdfkselfksdf<:fact>aaaasdasd</:fact>okpoioxicvxcv"

    test("split on facts and merge back") {
        val (facts, other) = patternGenerator.splitLineOnFactsAndOthersSubstrings(line, patternGenerator.extractFactsFromLine(line))
        val mergeResult = other.zip(facts.map(f => "<:fact>" + f + "</:fact>")).map(t => t._1 + t._2).mkString("") + other.last
        assertResult(line)(mergeResult)
    }
}
