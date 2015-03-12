package ru.amontag.inhound.pgenerator

import scala.annotation.tailrec

/**
 * Created by montag on 12.03.15.
 */
abstract class Pattern(val elements: Seq[ElementClass]) {
    def findMatchAll(line: String): List[String]

    def equalsTo(line: String): Boolean

    def stringRepresentation: String

    def compressedStringRepresentation: String
}

object RegexpPattern {
    val classes: Map[ElementClass, String] = Map(
        ElementClass(1) -> "\\d",
        ElementClass(2) -> "\\s",
        ElementClass(3) -> "\\w",
        ElementClass(4) -> "[-+=*\\<>%^]",
        ElementClass(5) -> "_",
        ElementClass(6) -> "\\+",
        ElementClass(7) -> "['\"]",
        ElementClass(8) -> "[.,;:?!]",
        ElementClass(9) -> "[#$&@~]",
        ElementClass(10) -> "[{}\\[\\]()]"
    ) ++ (1 to 10).map(i => ElementClass(i + 10) -> i.toString)
}

class RegexpPattern(elements: Seq[ElementClass]) extends Pattern(elements) {
    val regexp = elements.map(RegexpPattern.classes).mkString("").r

    override def findMatchAll(line: String): List[String] = regexp.findAllIn(line).toList

    override def equalsTo(line: String): Boolean = regexp.findFirstIn(line).isDefined

    override def stringRepresentation: String = regexp.regex

    override def compressedStringRepresentation: String = compress(elements.toList, "")

    @tailrec
    private def compress(elements: List[ElementClass], answer: String): String = {
        elements match {
            case Nil => answer
            case head :: tail =>
                val pattern: String = RegexpPattern.classes(head)
                val newAnswer = answer.lastOption match {
                    case None => pattern
                    case Some('+') if answer.substring(0, answer.length - 1).endsWith(pattern) => answer
                    case Some(_) if answer.endsWith(pattern) => answer + "+"
                    case _ => answer + pattern
                }
                compress(tail, newAnswer)
        }
    }
}

//new RegexpPattern((0 to 10).map(i => ElementClass(1))).compressedStringRepresentation