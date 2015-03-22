package ru.amontag.inhound.pgenerator.collection

/**
 * Created by montag on 22.03.15.
 */
object FactStripes {
    def apply(lines: List[List[String]]): FactStripes = apply(lines.map(line => 0 -> line), 0)
}

case class FactStripes(stripes: List[(Int, List[String])], column: Int) {
    def head = stripes map {
        case (shift, tokens) =>
            val dropLength = column - shift
            if (dropLength < 0) None
            else tokens.drop(dropLength).headOption
    }
    
    def next() = FactStripes(stripes, column + 1)
    
    def rShift(lineIndex: Int) = {
        val left = if(lineIndex == 0) Nil else stripes.take(lineIndex)
        val (centerShift, centerData) = stripes.drop(lineIndex).head
        val right = stripes.drop(lineIndex + 1)
        FactStripes(left ::: (centerShift + 1 -> centerData) :: right, column)
    }

    def lShift(lineIndex: Int) = {
        val left = if(lineIndex == 0) Nil else stripes.take(lineIndex).map(t => t._1 + 1 -> t._2)
        val center = stripes.drop(lineIndex).head
        val right = stripes.drop(lineIndex + 1).map(t => t._1 + 1 -> t._2)
        FactStripes(left ::: (center :: right), column)
    }
}