package ru.amontag.inhound.pgenerator

import scala.util.matching.Regex

/**
 * Created by montag on 17.03.15.
 */
object RegexpClassSet extends UniverseClassSet with Tokenizer {
    val stringRepresentationOfClasses: Map[ElementClass, String] = {
        val minimalClasses = Map(
            defaultClass,
            ElementClass(1) -> "\\d",
            ElementClass(2) -> "\\s",
            ElementClass(3) -> "\\w",
            ElementClass(4) -> "[-+=*\\<>%^]",
            ElementClass(5) -> "_",
            ElementClass(6) -> "\\+",
            ElementClass(7) -> "['\"]",
            ElementClass(8) -> "[.,;:?!]",
            ElementClass(9) -> "[#$&@~]",
            ElementClass(10) -> "[{}\\[\\]()]",
            ElementClass(11) -> "[a-z]",
            ElementClass(12) -> "[A-Z]",
            ElementClass(13) -> "[а-яё]",
            ElementClass(14) -> "[А-ЯЁ]"
        )
        val withNumbers = minimalClasses ++ (0 to 9).map(i => ElementClass(i + 1 + minimalClasses.size) -> i.toString)
        val enAlphabet = "abcdefghijklmnopqrstuvwxyz"
        val ruAlphabet = "абвгдежзийклмнопрстуфхцчшщъыьэюяё"
        withNumbers ++
          (enAlphabet + enAlphabet.toUpperCase + ruAlphabet + ruAlphabet.toUpperCase).toList.zipWithIndex
            .map({ case (letter, num) => ElementClass(num + 1 + withNumbers.size) -> letter.toString})
    }

    def defaultClass = ElementClass(-1) -> "."

    val classes: Map[Regex, ElementClass] = RegexpClassSet.stringRepresentationOfClasses.map({ case (ec, p) => (p.r, ec)}).toMap

    override def defineClass(token: String): Set[ElementClass] = {
        classes.map({ case (r, c) => r.findFirstIn(token).map(v => c)}).flatten.toList match {
            case Nil => Set(defaultClass._1)
            case answer => answer.toSet
        }
    }

    override def tokenize(line: String): Seq[String] = line.map(x => x.toString)
}