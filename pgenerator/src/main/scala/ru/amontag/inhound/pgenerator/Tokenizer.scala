package ru.amontag.inhound.pgenerator

/**
 * Created by montag on 17.03.15.
 */
trait Tokenizer {
    def tokenize(line: String): Seq[String]
}
