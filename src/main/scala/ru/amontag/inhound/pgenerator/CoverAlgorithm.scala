package ru.amontag.inhound.pgenerator

import scala.collection.mutable

/**
 * Created by montag on 10.03.15.
 */
object CoverAlgorithm {
    def functor(alpha: Double, betta: Double)(vertex: Vertex) = {
        val SoI: Double = vertex.joinedElements.size
        val CoL: Double = vertex.containsIn.size

        Math.pow(SoI, alpha) * Math.pow(CoL, betta)
    }

    def cover(sets: Seq[Set[Element]], functor: (Vertex) => Double): CoverResult = {
        val indexedElementsOfSets = sets.zipWithIndex
          .map({ case (set, indexOfLine) => set.map(element => (element, indexOfLine))}).flatten
          .groupBy({ case (element, containsInLine) => element})
          .map({ case (element, containsInLines) => (element, containsInLines.map(_._2).toSet)})

        val vertexes = indexedElementsOfSets.map({ case (element, containsIn) => Vertex(Set(element), containsIn)})

        val edges = vertexes.map(left => (left, vertexes.filter(right => (right.containsIn & left.containsIn).size > 0)))
          .map({ case (left, neighbors) => neighbors.filter(_ != left).map(new Edge(left, _, functor))})
          .flatten.toStream.distinct

        implicit val ordering = Ordering.by[Edge, Double](_.weight)
        val edgesQueue = mutable.PriorityQueue(edges: _*)

        edgesQueue.foreach(println)
        CoverResult(Nil, Nil)
    }

    case class CoverResult(accepted: List[Set[Element]], rejected: List[Set[Element]])

    case class Element(id: Int)

    case class Vertex(joinedElements: Set[Element],
                      containsIn: Set[Int])

    class Edge(val left: Vertex, val right: Vertex, f: (Vertex) => Double) {
        val probe: Vertex = Vertex(left.joinedElements ++ right.joinedElements, left.containsIn & right.containsIn)
        val weight: Double = 2 * f(probe) - f(left) - f(right)

        def canEqual(other: Any): Boolean = other.isInstanceOf[Edge]

        override def equals(other: Any): Boolean = other match {
            case that: Edge =>
                (that canEqual this) && ((left == that.left && right == that.right) || (left == that.right && right == that.left))
            case _ => false
        }

        override def hashCode(): Int = left.hashCode() ^ right.hashCode()

        override def toString: String = String.format("left: %s, right: %s, probe: %s, weight: %s",
            left.toString,
            right.toString,
            probe.toString,
            weight.toString)
    }

}
