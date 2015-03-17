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

class RegexpPattern(elements: Seq[ElementClass]) extends Pattern(elements) {
    val regexp = elements.map(RegexpClassSet.stringRepresentationOfClasses).mkString("").r

    override def findMatchAll(line: String): List[String] = regexp.findAllIn(line).toList

    override def equalsTo(line: String): Boolean = regexp.findFirstIn(line).isDefined

    override def stringRepresentation: String = regexp.regex

    override def compressedStringRepresentation: String = compress(elements.toList, "")

    @tailrec
    private def compress(elements: List[ElementClass], answer: String): String = {
        elements match {
            case Nil => answer
            case head :: tail =>
                val pattern: String = RegexpClassSet.stringRepresentationOfClasses(head)
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