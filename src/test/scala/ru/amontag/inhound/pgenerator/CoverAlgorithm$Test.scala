package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite

/**
 * Created by montag on 10.03.15.
 */
class CoverAlgorithm$Test extends FunSuite {
    test("Just view work of algo") {
        val sets: Seq[Set[Int]] = Seq(
            Set(1, 2, 3, 4),
            Set(2, 3, 4, 5),
            Set(3, 4, 5, 6),
            Set(4, 5, 6, 7)
        )

        CoverAlgorithm.cover(sets, CoverAlgorithm.functor[Int](0.5, 0.5))
    }
}
