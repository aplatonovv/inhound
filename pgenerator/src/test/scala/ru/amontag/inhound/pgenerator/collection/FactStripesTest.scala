package ru.amontag.inhound.pgenerator.collection

import org.scalatest.FunSuite

/**
 * Created by montag on 22.03.15.
 */
class FactStripesTest extends FunSuite {
    val lines = List(
        List(1,2,3,4,5).map(_.toString),
        List(1,2,3,4,5).map(_.toString),
        List(1,2,3,4,5).map(_.toString),
        List(1,2,3,4,5).map(_.toString)
    )

    val stripes = FactStripes(lines)

    test("head test") {
        assertResult(List(Some("1"),Some("1"),Some("1"),Some("1")))(stripes.head)
    }

    test("next") {
        val nextResult = stripes.next()
        assertResult(List(Some("2"),Some("2"),Some("2"),Some("2")))(nextResult.head)
    }

    test("rShift") {
        val res = stripes.rShift(1)
        assertResult(List(Some("1"),None,Some("1"),Some("1")))(res.head)
    }

    test("lShift") {
        val res = stripes.lShift(1)
        assertResult(List(None,Some("1"),None,None))(res.head)
    }

    test("lShiftRShiftNextAndHead") {
        val newStripes = stripes.rShift(0).lShift(1).next()
        val res = newStripes.head
        assertResult(List(None,Some("2"),Some("1"),Some("1")))(res)
        assertResult(List(Some("1"),Some("3"),Some("2"),Some("2")))(newStripes.next().head)
    }
}
