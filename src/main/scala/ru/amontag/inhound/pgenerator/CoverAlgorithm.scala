package ru.amontag.inhound.pgenerator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by montag on 10.03.15.
 */
object CoverAlgorithm {
    def functor(sizeOfIntersectionCoefficient: Double, countOfLinesCoefficient: Double)(vertex: Vertex) = {
        val SoI: Double = vertex.joinedElements.size
        val CoL: Double = vertex.containsIn.size

        Math.pow(SoI, sizeOfIntersectionCoefficient) * Math.pow(CoL, countOfLinesCoefficient)
    }

    def cover(sets: Seq[Set[Element]], functor: (Vertex) => Double, trace: Boolean = false): CoverResult = {
        val indexOfSets = sets.zipWithIndex
        val indexedElementsOfSets = indexOfSets
          .map({ case (set, indexOfLine) => set.map(element => (element, indexOfLine))}).flatten
          .groupBy({ case (element, containsInLine) => element})
          .map({ case (element, containsInLines) => (element, containsInLines.map(_._2).toSet)})

        val vertexes = indexedElementsOfSets.map({ case (element, containsIn) => Vertex(Set(element), containsIn, functor)})

        val conjunctionList = vertexes.map(left => (left, vertexes.filter(right => (right.containsIn & left.containsIn).size > 0)))
          .map({ case (left, neighbors) => left -> neighbors.filter(_ != left).map(new Edge(left, _)).toList})
          .toStream
        val edges = conjunctionList.map(_._2).flatten.distinct.toSet
        val conjunctionMap = mutable.Map(conjunctionList: _*)

        val answer = cover(edges, conjunctionMap, vertexes.maxBy(_.score), trace)
        val (accepted, rejected) = indexOfSets.partition(t => answer.containsIn(t._2))
        CoverResult(accepted.map(_._1).toList, rejected.map(_._1).toList)
    }

    @tailrec
    private def cover(edges: Set[Edge],
                      conjunctionList: mutable.Map[Vertex, List[Edge]],
                      lastDecision: Vertex,
                      trace: Boolean): Vertex = {
        if(trace) {
            edges.foreach(println)
            println("=======================================================================")
        }

        if (edges.size == 0) lastDecision
        else {
            val edge = edges.maxBy(_.weight)
            if (edge.weight < 0) lastDecision
            else {
                val newVertex = edge.probe
                val neighbors = (conjunctionList(edge.left) ::: conjunctionList(edge.right))
                      .map(_.right).distinct
                      .filter(v => v != edge.left && v != edge.right)
                val newEdges = neighbors.map(right => new Edge(newVertex, right))
                conjunctionList.remove(edge.left)
                conjunctionList.remove(edge.right)

                val oldEdges = ArrayBuffer[Edge](edge)
                neighbors.foreach(left => {
                    val forDeleteEdges = conjunctionList(left).filter(e => e.right == edge.left || e.right == edge.right)
                    val outEdges = conjunctionList(left).filter(e => e.right != edge.left && e.right != edge.right)
                    oldEdges ++= forDeleteEdges
                    conjunctionList.update(left, new Edge(left, newVertex) :: outEdges)
                })
                conjunctionList.put(newVertex, newEdges)
                val newEdgesSet: Set[Edge] = (edges -- oldEdges) ++ newEdges
                cover(newEdgesSet, conjunctionList, conjunctionList.keys.maxBy(_.score), trace)
            }
        }
    }

    case class CoverResult(accepted: List[Set[Element]], rejected: List[Set[Element]])

    case class Element(id: Int)

    case class Vertex(joinedElements: Set[Element],
                      containsIn: Set[Int],
                      functor: (Vertex) => Double) {
        val score = functor(this)

        override def toString: String = "joined: " + joinedElements + "\tcontainsIn: " + containsIn + "\tscore: " + score
    }

    class Edge(val left: Vertex, val right: Vertex) {
        val f = left.functor
        val probe: Vertex = Vertex(left.joinedElements ++ right.joinedElements, left.containsIn & right.containsIn, f)
        val weight: Double = 2 * probe.score - left.score - right.score

        def canEqual(other: Any): Boolean = other.isInstanceOf[Edge]

        override def equals(other: Any): Boolean = other match {
            case that: Edge =>
                (that canEqual this) && ((left == that.left && right == that.right) || (left == that.right && right == that.left))
            case _ => false
        }

        override def hashCode(): Int = left.hashCode() ^ right.hashCode()

        override def toString: String = String.format("left: %s\t right: %s\n\t\t weight: %s\t probe: %s",
            left.toString,
            right.toString,
            weight.toString,
            probe.toString)
    }

}