package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    //val length = 100000000
    val length = 10000
    //val length = 100
    val chars = new Array[Char](length)
    val threshold = 100
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanced(chars: Array[Char], open: Int): Boolean = {
      if (chars.length == 0) open == 0
      else if (chars(0) == '(') balanced(chars.tail, open + 1)
      else if (chars(0) == ')') open > 0 && balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)
    }
    balanced(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      def helper(chars: Array[Char], a1: Int, a2: Int): (Int, Int) = {
        if (chars.length == 0) (a1, a2)
        else if (chars(0) == '(') helper(chars.tail, a1+1, a2)
        else if (chars(0) == ')') {
          if (a1 > 0) helper(chars.tail, a1-1, a2)
          else helper(chars.tail, a1, a2+1)
        }
        else helper(chars.tail, a1, a2)
      }
      helper(chars.slice(idx, until), arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from,mid), reduce(mid,until))
        val cancel = Math.min(left._1,right._2)
        (left._1 + right._1 - cancel, left._2 + right._2 - cancel)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
