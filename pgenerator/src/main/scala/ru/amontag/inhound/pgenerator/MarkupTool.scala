package ru.amontag.inhound.pgenerator

import java.util.regex.Pattern

import scala.annotation.tailrec

/**
 * Created by montag on 19.03.15.
 */
object MarkupTool {
    val factMarkupPattern = "<:fact>(.*?)</:fact>".r

    def splitTextByFacts(text: Seq[String]): (List[String], List[String]) = {
        text.map(line => line -> extractFactsFromLine(line))
          .map(splitLineOnFactsAndOthersSubstrings)
          .reduce((left, right) => (left._1 ::: right._1) -> (left._2 ::: right._2))
    }

    def extractFactsFromLine(line: String) = factMarkupPattern.findAllIn(line).toList

    def splitLineOnFactsAndOthersSubstrings(lineAndFacts: (String, List[String])): (List[String], List[String]) = {
        val (line, facts) = lineAndFacts
        facts.map(_.replaceAll("</?:fact>", "")) -> splitLineByFacts(line, facts)
    }

    @tailrec
    private def splitLineByFacts(line: String, facts: List[String], buffer: List[String] = Nil): List[String] = {
        facts match {
            case Nil => (if (line.length > 0) line :: buffer else buffer).reverse
            case fact :: tail =>
                val i = line.indexOf(fact)
                val (beforeFact, afterFact) = (line.take(i), line.drop(i).replaceFirst(Pattern.quote(fact), ""))
                splitLineByFacts(afterFact, tail, beforeFact :: buffer)
        }
    }
}
