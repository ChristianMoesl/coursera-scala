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
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
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
    var count = 0

    for (char <- chars) {
      if (char == '(') count += 1
      else if (char == ')') {
        if (count == 0) return false
        count -= 1
      }
    }
    count == 0
  }


  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, open: Int, close: Int): (Int, Int) = {
      if (idx == until) (open, close)
      else {
        val (o, c) = { 
          if (chars(idx) == '(') (open + 1, close)
          else if (chars(idx) == ')') {
            if (open > 0) (open - 1, close) 
            else (open, close + 1)
          }
          else (open, close) 
        }

        traverse(idx + 1, until, o, c)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from  <= threshold)
        traverse(from, until, 0, 0) 
      else {
        val mid = (until - from) / 2 + from
       
        val ((lopen, lclose), (ropen, rclose)) = parallel(reduce(from, mid), reduce(mid, until))
        
        val closed = if (lopen >= rclose) rclose else lopen
        
        (lopen - closed + ropen, rclose - closed + lclose)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
