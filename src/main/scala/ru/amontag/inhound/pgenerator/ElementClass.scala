package ru.amontag.inhound.pgenerator

import scala.util.matching.Regex

/**
 * Created by montag on 12.03.15.
 */
case class ElementClass(id: Int)

abstract class UniverseClassSet {
    def defineClass(token: String): ElementClass
}

class RegexpClassSet extends UniverseClassSet {
    val classes: Map[Regex, ElementClass] = RegexpPattern.classes.map({case (ec, p) => (p.r, ec)}).toMap

    override def defineClass(token: String): ElementClass = {
        classes.find({case (regexp, ec) => regexp.findFirstIn(token).isDefined}).get._2
    }
}

