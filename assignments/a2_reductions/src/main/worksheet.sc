import scala.collection.mutable.Stack

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


/////////////////////////////////////////////////////////////
val input = Array[Float](0, 1, 3, 5, 1, 5, 7, 9, 15, 10)
val output = new Array[Float](10)

val tangentRatio = (v: Float, i: Int) => if (i > 0) then  (v.toFloat / i) else 0

def lineOfSight(input: Array[Float], output: Array[Float]): Unit =
  require(input.length == output.length, "Input and output must have equal length")

  def _updateOutput(v: Float, i: Int, m: Float, o: Array[Float]): Float =
    val newMax = tangentRatio(v, i).max(m)
    o(i) = newMax
    newMax

  var maxSoFar = 0.toFloat
  for ((v, i) <- input.zipWithIndex) {
    maxSoFar = _updateOutput(v, i, maxSoFar, output)
  }

lineOfSight(input, output)

assert(output.max == 1.875)

def upsweepSequential(input: Array[Float], from: Int, until: Int): Float =
  input.slice(from, until).zipWithIndex.map((v, i) => tangentRatio(v, from + i)).max

assert(upsweepSequential(input, 0, input.length) == 1.875)
assert(upsweepSequential(input, 1, 2) == 1)


enum Tree(val maxPrevious: Float):
  case Node(left: Tree, right: Tree) extends Tree(left.maxPrevious.max(right.maxPrevious))
  case Leaf(from: Int, until: Int, override val maxPrevious: Float) extends Tree(maxPrevious)

def upsweep(input: Array[Float], from: Int, end: Int,
            threshold: Int): Tree =
  val delta = end - from
  val midpoint = from + (delta / 2)
  delta match {
    case leaf if delta < threshold => Tree.Leaf(from, end, upsweepSequential(input, from, end))
    case _ =>
      val par = parallel(
        upsweep(input, from, midpoint, threshold),
        upsweep(input, midpoint, end, threshold)
      )
      Tree.Node(par._1, par._2)
  }

val testTree = upsweep(input, 0, input.length, 3)


def downsweepSequential(input: Array[Float], output: Array[Float],
                        startingAngle: Float, from: Int, until: Int): Unit =

  val useMax = upsweepSequential(input, from, until).max(startingAngle)
  for (i <- from until until)
      output(i) = useMax

def downsweep(input: Array[Float], output: Array[Float],
              startingAngle: Float, tree: Tree): Unit =
  tree match {
    case Tree.Node(left, right) =>
      parallel(
        downsweep(input, output, startingAngle, left),
        // right nodes use a starting value of left.maxPrevious.max(startingAngle)
        downsweep(input, output, left.maxPrevious.max(startingAngle), right),
      )
    case Tree.Leaf(from, until, _) =>
      downsweepSequential(input, output, startingAngle, from, until)
  }


/** Compute the line-of-sight in parallel. */
def parLineOfSight(input: Array[Float], output: Array[Float],
                   threshold: Int): Unit =

  val tree = upsweep(input, 0, input.length, threshold)
  downsweep(input, output, 0, tree)


val threshold = 5
val output1 = new Array[Float](input.length)
val output2 = new Array[Float](input.length)

parLineOfSight(input, output1, threshold)
lineOfSight(input, output2)

// Sequential
val testInput = Array(0f, 1f, 8f, 9f)
val testOutput = new Array[Float](testInput.length)
val expectedOutput = Array(0f, 1f, 4f, 4f)
lineOfSight(testInput, testOutput)
assert(testOutput.toList == expectedOutput.toList)

// Parallel
val testOutput2 = new Array[Float](testInput.length)
parLineOfSight(testInput, testOutput2, 2)
assert(testOutput2.toList == expectedOutput.toList)
