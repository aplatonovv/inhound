package ru.amontag.inhound.pgenerator

import scala.util.matching.Regex

/**
 * Created by montag on 17.03.15.
 */
object RegexpClassSet extends UniverseClassSet with Tokenizer {
    val stringRepresentationOfClasses: Map[ElementClass, String] = {
        val minimalClasses = Map(
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
        )
        val withNumbers = minimalClasses ++ (1 to 10).map(i => ElementClass(i + minimalClasses.size) -> i.toString)
        val enAlphabet = "abcdefghijklmnopqrstuvwxyz"
        val ruAlphabet = "абвгдежзийклмнопрстуфхцчшщъыьэюяё"
        withNumbers ++
          (enAlphabet + enAlphabet.toUpperCase + ruAlphabet + ruAlphabet.toUpperCase).toList.zipWithIndex
            .map({case (letter, num) => ElementClass(num + withNumbers.size) -> letter.toString})
    }

    val defaultClass = ElementClass(-1) -> "."

    val classes: Map[Regex, ElementClass] = RegexpClassSet.stringRepresentationOfClasses.map({case (ec, p) => (p.r, ec)}).toMap

    override def defineClass(token: String): Option[ElementClass] = {
        classes.find({case (regexp, ec) => regexp.findFirstIn(token).isDefined}).map(_._2)
    }

    override def tokenize(line: String): Seq[String] = line.map(x => x.toString)
}