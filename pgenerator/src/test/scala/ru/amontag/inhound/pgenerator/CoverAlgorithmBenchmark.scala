package ru.amontag.inhound.pgenerator

import java.io.{FileOutputStream, PrintWriter}

import org.scalatest.FunSuite
import resource._
import ru.amontag.inhound.pgenerator.CoverAlgorithm.{CoverResult, Element}

import scala.util.Random

/**
 * Created by montag on 16.03.15.
 */
class CoverAlgorithmBenchmark extends FunSuite {
    test("Test raise classes and sentences from 10 to 10000") {
        managed(new FileOutputStream("CoverAlgorithmBenchmark_3.tsv"))
          .flatMap(stream => managed(new PrintWriter(stream)))
          .foreach(writer => {
            writer.println("Count of classes\tCount of sentences\tMeasures")
            val functor = CoverAlgorithm.functor(0.5, 0.5)(_)

            for (classCount <- 910 to(1000, 100); sentenceCount <- 4510 to(10000, 500)) {
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
                writer.flush()
            }

            val classCount = 1000
            for (sentenceCount <- 10 to(10000, 500)) {
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
                writer.flush()
            }
        })
    }

    private def randomTake(n: Int, array: Array[Element]) =
        (for (i <- 0 until n) yield {
            array(Random.nextInt(array.length))
        }).toSet
}
