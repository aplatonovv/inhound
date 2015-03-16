package ru.amontag.inhound.pgenerator

import java.io.{PrintWriter, BufferedOutputStream, FileOutputStream}

import org.scalatest.FunSuite
import ru.amontag.inhound.pgenerator.CoverAlgorithm.{CoverResult, Element}
import resource._
import scala.util.Random

/**
 * Created by montag on 16.03.15.
 */
class CoverAlgorithmBenchmark extends FunSuite {
    test("Test raise classes and sentences from 10 to 100000") {
        managed(new FileOutputStream("CoverAlgorithmBenchmark.tsv"))
          .flatMap(stream => managed(new BufferedOutputStream(stream)))
          .flatMap(stream => managed(new PrintWriter(stream)))
          .foreach(writer => {
            writer.println("Count of classes\tCount of sentences\tMeasures")
            val functor = CoverAlgorithm.functor(0.5, 0.5)(_)

            for (classCount <- 10 to(100000, 100); sentenceCount <- 10 to(100000, 100)) {
                println("Classes: " + classCount + "\tSentences: " + sentenceCount)
                Random.setSeed(0)
                val classes = (for (i <- 1 to classCount) yield {
                    new Element(i) {}
                }).toArray
                val sets = (for (i <- 1 to sentenceCount) yield {
                    randomTake(5, classes)
                }).toArray

                val durations = Benchmark.repeat[CoverResult](() =>
                    CoverAlgorithm.cover(sets, functor, false)
                )(500).map(_.toMillis).mkString("\t")

                writer.println(String.format("%s\t%s\t%s",
                    classCount.toString,
                    sentenceCount.toString,
                    durations))
            }
        })
    }

    private def randomTake(n: Int, array: Array[Element]) =
        (for (i <- 0 until n) yield {
            array(Random.nextInt(array.length))
        }).toSet
}
