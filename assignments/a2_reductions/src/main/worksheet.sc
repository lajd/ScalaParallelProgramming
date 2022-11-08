import scala.collection.mutable.Stack

import reductions._

object ParallelParenthesesBalancing:
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



val chars = Array('(', ')', '(', ')', ')', '(', '(', '(', ')', ')')
ParallelParenthesesBalancing.balance(chars)
ParallelParenthesesBalancing.parBalance(Array('(', ')', ')'), 1)

val segmentStacks1 = ParallelParenthesesBalancing._parTraverse(Array('(', ')', '(', ')', '('), 2, 0, 5)
val reducedStacks1 = ParallelParenthesesBalancing._reduceStacks(segmentStacks1)

assert(reducedStacks1 == Stack('('))

val longLength = 100000000
val longChars = new Array[Char](longLength)
val longThreshold = 100000


longChars(500) = '('
longChars(5000) = ')'
longChars(50000) = '('
longChars(500000) = ')'
longChars(5000000) = '('
val segmentStacks2 = ParallelParenthesesBalancing._parTraverse(longChars, longThreshold, 0, longChars.length)
val reducedStacks2= ParallelParenthesesBalancing._reduceStacks(segmentStacks2)

assert(reducedStacks2 == Stack('('))

val t0 = System.nanoTime()
ParallelParenthesesBalancing.parBalance(longChars, longThreshold)
val t1 = System.nanoTime()
val parElapsed = (t1 - t0) / (1.0e6)

println(s"Parallel Elapsed: $parElapsed ms")

val t2 = System.nanoTime()
ParallelParenthesesBalancing.balance(longChars)
val t3 = System.nanoTime()
val seqElapsed = (t3 - t2) / (1.0e6)

println(s"Sequential Elapsed: $seqElapsed ms")
