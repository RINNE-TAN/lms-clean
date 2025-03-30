package lms
package core
import stub._
import macros.SourceContext
import lms.collection.mutable.ArrayOps

class KMPTest extends TutorialFunSuite {
  val under = "kmp/"
  test("kmp") {
    val driver = new DslDriver[Unit, Int] with ArrayOps {

      @virtualize
      def naiveMatch(
          pattern: Array[Int],
          text: Array[Int],
          j: Int,
          k: Int
      ): Int = {
        if (pattern.length == j) {
          k - j
        } else if (text.length == k) {
          -1
        } else if (pattern(j) == text(k)) {
          naiveMatch(pattern, text, j + 1, k + 1)
        } else {
          val s = rematch(pattern, j, 0, 1)
          if (s == -1) {
            naiveMatch(pattern, text, 0, k + 1)
          } else {
            naiveMatch(pattern, text, s, k)
          }
        }
      }

      @virtualize
      def rematch(pattern: Array[Int], i: Int, jp: Int, kp: Int): Int = {
        if (i == 0) {
          -1
        } else {
          if (kp == i) { jp }
          else {
            if (pattern(jp) == pattern(kp)) {
              rematch(pattern, i, jp + 1, kp + 1)
            } else {
              rematch(pattern, i, 0, (kp - jp + 1))
            }
          }
        }
      }

      @virtualize
      def snippet(u: Rep[Unit]) =
        naiveMatch(
          List(1, 2, 1).toArray,
          List(5, 4, 6, 1, 2, 1, 3).toArray,
          0,
          0
        )
    }
    val src = driver.code
    checkOut(
      "kmp",
      "scala", {
        println(src)
      }
    )
  }

  test("kmp_stage") {
    val driver = new DslDriver[Array[Int], Int] with ArrayOps {

      @virtualize
      def stageMatch(
          pattern: Array[Int],
          j: Int
      ): Rep[(Int, Array[Int]) => Int] = fun { (k, text) =>
        if (pattern.length == j) {
          k - j
        } else if (text.length == k) {
          -1
        } else if (pattern(j) == text(k)) {
          stageMatch(pattern, j + 1)(k + 1, text)
        } else {
          val s = rematch(pattern, j, 0, 1)
          if (s == -1) {
            stageMatch(pattern, 0)(k + 1, text)
          } else {
            stageMatch(pattern, s)(k, text)
          }
        }
      }

      @virtualize
      def rematch(pattern: Array[Int], i: Int, jp: Int, kp: Int): Int = {
        if (i == 0) {
          -1
        } else {
          if (kp == i) { jp }
          else {
            if (pattern(jp) == pattern(kp)) {
              rematch(pattern, i, jp + 1, kp + 1)
            } else {
              rematch(pattern, i, 0, (kp - jp + 1))
            }
          }
        }
      }

      @virtualize
      def snippet(text: Rep[Array[Int]]) =
        stageMatch(List(1, 2, 3, 1, 2, 4, 1, 2, 3, 1, 2, 3).toArray, 0)(0, text)
    }
    val src = driver.code
    checkOut(
      "kmp_stage",
      "scala", {
        println(src)
      }
    )
  }
}
