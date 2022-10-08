package scalashop

import org.scalameter.*

object HorizontalBoxBlurRunner:

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val radius = 3
    val width = 1920
    val height = 1080
    val src = Img(width, height)
    val dst = Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface:

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    val minY = from.max(0)
    val maxY = end.min(src.height)
    for j <- minY until maxY
    i <- 0 until src.width  // Traverse top to bottom
    yield dst.update(i, j,  boxBlurKernel(src, i, j, radius))

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit =
    val rowsPertask = (src.height / numTasks).max(1)
    val startEndRanges = Range(0, src.height, rowsPertask).map(x => (x, (x + rowsPertask).min(src.height)))

    val tasks = startEndRanges.map((start, end) => {
      task {
        blur(src, dst, start, end, radius)
      }
    })

    tasks.map(t => t.join())
