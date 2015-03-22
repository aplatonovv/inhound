package ru.amontag.inhound.pgenerator

import java.io.{PrintStream, FileOutputStream, PrintWriter}

import org.scalatest.FunSuite
import resource._
import ru.amontag.inhound.pgenerator.CoverAlgorithm.{CoverResult, Element}

import scala.io.Source
import scala.util.Random

/**
 * Created by montag on 16.03.15.
 */
class CoverAlgorithmBenchmark extends FunSuite {
    test("Test raise classes and sentences from 10 to 10000") {
        managed(new FileOutputStream("CoverAlgorithmBenchmark_4.tsv"))
          .flatMap(stream => managed(new PrintWriter(stream)))
          .foreach(writer => {
            writer.println("Count of classes\tCount of sentences\tMeasures")
            val functor = CoverAlgorithm.functor(0.5, 0.5)(_)

            for (classCount <- 10 to (1010, 100); sentenceCount <- 10 to(10010, 500)) {
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

    test("post processing benchmark data") {
        managed(new FileOutputStream("CoverAlgorithmBenchmark.means.tsv"))
          .flatMap(s => managed(new PrintStream(s)))
          .foreach(writer => {
            writer.println("CountOfClasses\tCountOfSentences\tMean")
            Source.fromFile("CoverAlgorithmBenchmark.out.tsv").getLines().drop(2)
              .filterNot(_.contains("Measures"))
              .map(_.split('\t').toList)
              .map(l => {
                val (countOfClasses :: countOfSentences :: tail) = l; (countOfClasses.toInt, countOfSentences.toInt, tail)})
              .map(t => (t._1, t._2, t._3.map(_.toDouble)))
              .map(t => (t._1, t._2, t._3.sum / t._3.size)).toList
              .sortBy(_._1)
              .foreach(t => {
                val (countOfClasses, countOfSentences, mean) = t
                writer.println(List(countOfClasses, countOfSentences, mean).mkString("\t"))
            })
        })
    }

    private def randomTake(n: Int, array: Array[Element]) =
        (for (i <- 0 until n) yield {
            array(Random.nextInt(array.length))
        }).toSet
}
