package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite
import ru.amontag.inhound.pgenerator.Benchmark.Generator

/**
 * Created by montag on 13.03.15.
 */
class BenchmarkTest extends FunSuite {
    test("100 repeats") {
        val results = Benchmark.repeat(() => {
            val a = 1
            val b = 2
            a + b
        })()
        assertResult(100)(results.size)
    }

    test("200 repeats") {
        val results = Benchmark.repeat(() => {
            val a = 1
            val b = 2
            a + b
        })(200)
        assertResult(200)(results.size)
    }

    test("Range map") {
        val generator = Generator(0, 100, 1, x => x)
        val results = Benchmark.range(generator, (x: Int) => {
            val a = 1
            val b = 2
            a + b
        })(150)
        assertResult(100)(results.keySet.size)
        assertResult(150)(results(0).size)
    }
}
