package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite
import ru.amontag.inhound.pgenerator.CoverAlgorithm.{Vertex, Edge, Element}

/**
 * Created by montag on 10.03.15.
 */
class CoverAlgorithm$Test extends FunSuite {
    test("Edge equality") {
        val e1 = new Edge(Vertex(Set(Element(1), Element(2)), Set(0, 1, 2)), Vertex(Set(Element(2), Element(3)), Set(0, 1, 2)), x => 0.0)
        val e2 = new Edge(Vertex(Set(Element(2), Element(3)), Set(0, 1, 2)), Vertex(Set(Element(1), Element(2)), Set(0, 1, 2)), x => 0.0)
        assert(e1 == e2)
    }

    test("Just view work of algo") {
        val sets: Seq[Set[Element]] = Seq(
            Set(1, 2, 3, 4),
            Set(2, 3, 4, 5),
            Set(3, 4, 5, 6),
            Set(4, 5, 6, 7)
        ).map(_.map(Element))

        CoverAlgorithm.cover(sets, CoverAlgorithm.functor(0.5, 0.5))
    }
}
