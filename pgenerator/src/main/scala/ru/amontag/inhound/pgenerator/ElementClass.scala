package ru.amontag.inhound.pgenerator

/**
 * Created by montag on 12.03.15.
 */
case class ElementClass(id: Int)

abstract class UniverseClassSet {
    def defineClass(token: String): Option[ElementClass]

    val defaultClass: (ElementClass, String)
}



