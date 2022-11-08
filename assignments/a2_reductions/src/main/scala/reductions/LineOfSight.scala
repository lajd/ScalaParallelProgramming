package reductions

import org.scalameter.*

object LineOfSightRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 100,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

enum Tree(val maxPrevious: Float):
  case Node(left: Tree, right: Tree) extends Tree(left.maxPrevious.max(right.maxPrevious))
  case Leaf(from: Int, until: Int, override val maxPrevious: Float) extends Tree(maxPrevious)

object LineOfSight extends LineOfSightInterface:

  val tangentRatio = (v: Float, i: Int) => if (i > 0) then  (v.toFloat / i) else 0

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit =
    require(input.length == output.length, "Input and output must have equal length")

    def _updateOutput(v: Float, i: Int, m: Float, o: Array[Float]): Float =
      val newMax = tangentRatio(v, i).max(m)
      o(i) = newMax
      newMax

    var maxSoFar = 0f
    for ((v, i) <- input.zipWithIndex) {
      maxSoFar = _updateOutput(v, i, maxSoFar, output)
    }

  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float =
    input.slice(from, until).zipWithIndex.map((v, i) => tangentRatio(v, from + i)).max

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Tree.Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Tree.Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
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

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit =

    val useMax = upsweepSequential(input, from, until).max(startingAngle)
    for (i <- from until until)
      output(i) = useMax

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
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

