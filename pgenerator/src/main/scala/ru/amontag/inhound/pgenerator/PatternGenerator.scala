package ru.amontag.inhound.pgenerator

/**
 * Created by montag on 17.03.15.
 */
abstract class PatternGenerator {
    def mine(lines: Seq[String]): List[Pattern] = {
        val (facts, otherLines) = MarkupTool.splitTextByFacts(lines)
        mine(facts, otherLines)
    }

    protected def mine(facts: Seq[String], otherLines: Seq[String]): List[Pattern]
}
