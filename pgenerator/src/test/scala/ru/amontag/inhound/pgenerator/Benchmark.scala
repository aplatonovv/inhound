package ru.amontag.inhound.pgenerator

import scala.concurrent.duration.Duration
import scala.concurrent.duration

/**
 * Created by montag on 13.03.15.
 */
object Benchmark {
    def repeat[T](block: () => T)(repeats: Int = 100): List[Duration] = {
        (0 until repeats).map(i => {
            val startTime = System.nanoTime()
            block()
            Duration(System.nanoTime() - startTime, duration.NANOSECONDS)
        }).toList
    }

    def range[K, T](generator: Generator[K], block: (K) => T)(repeats: Int = 100): Map[Int, List[Duration]] = {
        generator.work(value => {
            (0 until repeats).map(i => {
                val startTime = System.nanoTime()
                block(value)
                Duration(System.nanoTime() - startTime, duration.NANOSECONDS)
            }).toList
        })
    }

    case class Generator[T](from: Int, to: Int, step: Int, producer: Int => T) {
        def work[K](f: (T) => K): Map[Int, K] = {
            (from until(to, step)).map(i => {
                i -> f(producer(i))
            }).toMap
        }
    }
}