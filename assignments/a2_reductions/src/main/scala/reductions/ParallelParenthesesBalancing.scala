package reductions

import scala.annotation.*
import org.scalameter.*

import scala.collection.mutable.{Map, Stack}

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:
  val openBracket: Char = '('
  val closedBracket: Char = ')'

  def _updateStack(char: Char, stack: Stack[Char]): Stack[Char] =
    if (char == openBracket) {
      // Open brackets always get appended
      // They cannot cancel out a closed bracket
      stack.push(openBracket)
    } else if (char == closedBracket) {
      if (stack.nonEmpty && stack.head == openBracket) {
        // Cancel out the last closed bracket
        // by popping it from the stack
        stack.pop()
      } else {
        stack.push(closedBracket)
      }
    }
    stack

  def _getSegmentStack(chars: Array[Char], startIdx: Int, endIdx: Int): Stack[Char] =
    // Traverse the segment of the array, returning a stack
    // of non-cancelled-out parentheses
    var stack = new Stack[Char]()
    for (i <- startIdx until endIdx)
      stack = _updateStack(chars(i), stack)
    stack

  def balance(chars: Array[Char]): Boolean =
    val stack = _getSegmentStack(chars, 0, chars.length)
    stack.isEmpty

  def _reduceStacks(bracketList: List[Char]): Stack[Char] =
    var stack = new Stack[Char]()
    for (char <- bracketList) {
      stack = _updateStack(char, stack)
    }
    stack

  def _parTraverse(chars: Array[Char], threshold: Int, startIdx: Int, endIdx: Int): List[Char] =
    val segmentLength = endIdx - startIdx
    val midPoint = startIdx + segmentLength / 2

    segmentLength match {
      case divide if (segmentLength > threshold) =>
        // Divide in half
        val par = parallel(
          _parTraverse(chars, threshold, startIdx, midPoint),
          _parTraverse(chars, threshold, midPoint, endIdx),
        )
        par._1.toList ++ par._2.toList
      case _ => _getSegmentStack(chars, startIdx, endIdx).toList
    }


  def parBalance(chars: Array[Char], threshold: Int): Boolean =
    val joinedSegmentStacks = _parTraverse(
      chars, threshold, 0, chars.length
    )

    val stack = _reduceStacks(joinedSegmentStacks)
    stack.isEmpty

  // For those who want more:
  // Prove that your reduction operator is associative!

