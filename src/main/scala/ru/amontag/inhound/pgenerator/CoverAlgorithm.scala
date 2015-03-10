package ru.amontag.inhound.pgenerator

import ru.amontag.inhound.pgenerator.Graph.{Edge, MultiVertex}

/**
 * Created by montag on 10.03.15.
 */
object CoverAlgorithm {
    def functor[T](alpha: Double, betta: Double)(vertex: MultiVertex[T]) = {
        val SoI: Double = vertex.joinedElements.size
        val CoL: Double = vertex.containsIn.size

        Math.pow(SoI, alpha) * Math.pow(CoL, betta)
    }

    def cover[T](sets: Seq[Set[T]], functor: (MultiVertex[T]) => Double): CoverResult[T] = {
        val indexedElementsOfSets = sets.zipWithIndex
          .map({ case (set, indexOfLine) => set.map(element => (element, indexOfLine))}).flatten
          .groupBy({ case (element, containsInLine) => element})
          .map({ case (element, containsInLines) => (element, containsInLines.map(_._2).toSet)})

        val vertexes = indexedElementsOfSets.map({ case (element, containsIn) => MultiVertex()(Set(element), containsIn)})

        val edges = vertexes.map(left => (left, vertexes.filter(right => (right.containsIn & left.containsIn).size > 0)))
          .map({ case (left, neighbors) => neighbors.map(edge(left, _, functor))}).flatten

        edges.foreach(println)
        CoverResult(Nil, Nil)
    }

    private def edge[T](left: MultiVertex[T], right: MultiVertex[T], functor: (MultiVertex[T]) => Double) = {
        val leftWeight = functor(left)
        val rightWeight = functor(right)
        val probe = MultiVertex()(left.joinedElements ++ right.joinedElements, left.containsIn & right.containsIn)
        val probeWeight = functor(probe)

        Edge(left, right, 2*probeWeight - (leftWeight + rightWeight), probe)
    }

    case class CoverResult[T](accepted: List[Set[T]], rejected: List[Set[T]])

}
