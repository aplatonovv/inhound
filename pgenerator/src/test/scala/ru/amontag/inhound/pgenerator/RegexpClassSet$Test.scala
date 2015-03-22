package ru.amontag.inhound.pgenerator

import org.scalatest.FunSuite
import ru.amontag.inhound.pgenerator.model.RegexpClassSet

/**
 * Created by montag on 18.03.15.
 */
class RegexpClassSet$Test extends FunSuite {
    test("Parse numbers") {
        val line = "1234567890"
        val parsingResult = RegexpClassSet.tokenize(line)
          .map(RegexpClassSet.defineClass)
          .map(_.map(e => RegexpClassSet.stringRepresentationOfClasses(e))).toSet

        assertResult("1,2,3,4,5,6,7,8,9,0".split(",").map(i => Set(i, "\\w", "\\d", ".")).toSet)(parsingResult)
    }

    test("Parse only small letters") {
        val line = "qwertyuiopasdfghjklzxcvbnm"
        val parsingResult = RegexpClassSet.tokenize(line)
          .map(RegexpClassSet.defineClass)
          .map(_.map(e => RegexpClassSet.stringRepresentationOfClasses(e))).toSet

        val expected = "q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m".split(",")
          .map(i => Set(i, "\\w", "[a-z]", ".")).toSet
        assertResult(expected)(parsingResult)
    }

    test("Parse only capital letters") {
        val line = "qwertyuiopasdfghjklzxcvbnm".toUpperCase()
        val parsingResult = RegexpClassSet.tokenize(line)
          .map(RegexpClassSet.defineClass)
          .map(_.map(e => RegexpClassSet.stringRepresentationOfClasses(e))).toSet

        val expected = "q,w,e,r,t,y,u,i,o,p,a,s,d,f,g,h,j,k,l,z,x,c,v,b,n,m".toUpperCase().split(",")
          .map(i => Set(i, "\\w", "[A-Z]", ".")).toSet
        assertResult(expected)(parsingResult)
    }

    test("Parse simple line") {
        val line = "Hello! I'm a Nick :), а я Ваня404"
        val parsingResult = RegexpClassSet.tokenize(line)
          .map(RegexpClassSet.defineClass)
          .map(_.map(e => RegexpClassSet.stringRepresentationOfClasses(e))).toList

        val p1 = List(Set("H", "\\w", "[A-Z]", "."))
        val p2 = "e,l,l,o".split(",").map(i => Set(i, "\\w", "[a-z]", ".")).toList
        val p3 = List(Set("[.,;:?!]", "."))
        val p4 = List(Set("\\s", "."))
        val p5 = List(Set("I", "\\w", "[A-Z]", "."))
        val p6 = List(Set("['\"]", "."))
        val p7 = List(Set("m", "\\w", "[a-z]", "."))
        val p8 = List(Set("\\s", "."))
        val p9 = List(Set("a", "\\w", "[a-z]", "."))
        val p10 = List(Set("\\s", "."))
        val p11 = List(Set("N", "\\w", "[A-Z]", "."))
        val p12 = "i,c,k".split(",").map(i => Set(i, "\\w", "[a-z]", ".")).toList
        val p13 = List(Set("\\s", "."))
        val p14 = List(Set("[.,;:?!]", "."))
        val p15 = List(Set("[{}\\[\\]()]", "."))
        val p16 = List(Set("[.,;:?!]", "."))
        val p17 = List(Set("\\s", "."))
        val p18 = List(Set("а", "[а-яё]", "."))
        val p19 = List(Set("\\s", "."))
        val p20 = List(Set("я", "[а-яё]", "."))
        val p21 = List(Set("\\s", "."))
        val p22 = List(Set("В", "[А-ЯЁ]", "."))
        val p23 = "а,н,я".split(",").map(i => Set(i, "[а-яё]", ".")).toList
        val p24 = "4,0,4".split(",").map(i => Set(i, "\\d", "\\w", ".")).toList


        val expected = p1 ++ p2 ++ p3 ++ p4 ++ p5 ++ p6 ++ p7 ++ p8 ++ p9 ++ p10 ++ p11 ++ p12 ++ p13 ++ p14 ++ p15 ++ p16 ++ p17 ++ p18 ++ p19 ++ p20 ++ p21 ++ p22 ++ p23 ++ p24
        (expected zip parsingResult).foreach(t => assertResult(t._1)(t._2))
    }
}
