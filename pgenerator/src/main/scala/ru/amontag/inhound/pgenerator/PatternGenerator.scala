package ru.amontag.inhound.pgenerator

/**
 * Created by montag on 17.03.15.
 */
abstract class PatternGenerator {
    def mine(lines: Seq[String]): List[Pattern]
}
