package ru.amontag.inhound.pgenerator

/**
 * Created by montag on 10.03.15.
 */
object Graph {
    case class MultiVertex[T]()(val joinedElements: Set[T],
                                val containsIn: Set[Int])
    case class Edge[T](left: MultiVertex[T], right: MultiVertex[T], weight: Double, probe: MultiVertex[T])
}
