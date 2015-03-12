package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite
import ru.amontag.inhound.pgenerator.CoverAlgorithm.Element
import scala.util.Random

/**
 * Created by montag on 10.03.15.
 */
class CoverAlgorithm$Test extends FunSuite {
    test("Just view work of algo") {
        Random.setSeed(0)
        val classes = (for (i <- 1 to 10) yield {
            new Element(i) {}
        }).toArray
        val sets = (for (i <- 1 to 1000) yield {
            randomTake(5, classes)
        }).toArray

//        val wc1 = new Element(1)
//        val wc2 = new Element(2)
//        val wc3 = new Element(3)
//        val wc4 = new Element(4)
//        val wc5 = new Element(5)
//        val wc6 = new Element(6)
//        val sets = Array(
//            Set(wc2, wc3, wc6),       //0
//            Set(wc2, wc3, wc6),       //1
//            Set(wc1, wc2, wc3, wc4),  //2
//            Set(wc1, wc2, wc3, wc4),  //3
//            Set(wc1, wc2, wc3, wc4),  //4
//            Set(wc3, wc4, wc5)        //5
//        )

        println("=================================")
        val functor = CoverAlgorithm.functor(4, 0.25)(_)
        val answer = CoverAlgorithm.cover(sets, functor, false)
        println("Result = " + answer.accepted)
        val CoL = answer.accepted.size.toString
        val SoI = answer.accepted.reduce(_ & _).size.toString
        println(String.format("CoL = %s;\tSoI = %s", CoL, SoI))
    }

    private def randomTake(n: Int, array: Array[Element]) =
        (for (i <- 0 until n) yield {
            array(Random.nextInt(array.length))
        }).toSet
}
