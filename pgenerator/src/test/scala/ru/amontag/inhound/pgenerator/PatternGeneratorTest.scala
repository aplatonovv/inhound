package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite
import ru.amontag.inhound.pgenerator.markup.MarkupTool

/**
 * Created by montag on 19.03.15.
 */
class MarkupToolTest extends FunSuite {
    val line = "kdlfksldkfsdf<:fact>987398371243</:fact>;kdfkselfksdf<:fact>aaaasdasd</:fact>okpoioxicvxcv"

    test("split on facts and merge back") {
        val (facts, other) = MarkupTool.splitLineOnFactsAndOthersSubstrings(line, MarkupTool.extractFactsFromLine(line))
        val mergeResult = other.zip(facts.map(f => "<:fact>" + f + "</:fact>")).map(t => t._1 + t._2).mkString("") + other.last
        assertResult(line)(mergeResult)
    }
}
