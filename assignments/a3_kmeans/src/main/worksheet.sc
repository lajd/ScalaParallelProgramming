import scala.annotation.tailrec
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.{ForkJoinTaskSupport, ParMap, ParSeq}
import scala.util.Random
import org.scalameter.*

import java.util.concurrent.ForkJoinPool
import scala.util.Random


class Point(val x: Double, val y: Double, val z: Double):
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double =
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"


def generatePoints(k: Int, num: Int): ParSeq[Point] =
  val randx = Random(1)
  val randy = Random(3)
  val randz = Random(5)
  (0 until num)
    .map({ i =>
      val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
      val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
      val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
      Point(x, y, z)
    }).to(mutable.ArrayBuffer).par

def initializeMeans(k: Int, points: ParSeq[Point]): ParSeq[Point] =
  val rand = Random(7)
  (0 until k).map(_ => points(rand.nextInt(points.length))).to(mutable.ArrayBuffer).par

def findClosest(p: Point, means: IterableOnce[Point]): Point =
  val it = means.iterator
  assert(it.nonEmpty)
  var closest = it.next()
  var minDistance = p.squareDistance(closest)
  while it.hasNext do
    val point = it.next()
    val distance = p.squareDistance(point)
    if distance < minDistance then
      minDistance = distance
      closest = point
  closest

def classify(points: Seq[Point], means: Seq[Point]): Map[Point, Seq[Point]] =
  val retMap = means.map(p => (p, List[Point]())).toMap
  val meansMap = points.groupBy(findClosest(_, means))
  retMap ++ meansMap

def classify(points: ParSeq[Point], means: ParSeq[Point]): ParMap[Point, ParSeq[Point]] =
  // This is the same as the sequential case -- the scala parallel data structures handle the rest
  val retMap = means.map(p => (p, ParSeq[Point]())).toMap
  val meansMap = points.groupBy(findClosest(_, means))
  retMap ++ meansMap

val points_ = List(Point(1, 2, 3), Point(3, 4, 5), Point(6, 7, 8), Point(10, 12, 13))
val means_ = List(Point(1, 1, 1), Point(3, 3, 3))

val classified_ = classify(points_, means_)
assert(classified_.size == means_.length)

def findAverage(oldMean: Point, points: Seq[Point]): Point = if points.isEmpty then oldMean else
  var x = 0.0
  var y = 0.0
  var z = 0.0
  points.foreach { p =>
    x += p.x
    y += p.y
    z += p.z
  }
  Point(x / points.length, y / points.length, z / points.length)

def findAverage(oldMean: Point, points: ParSeq[Point]): Point = if points.isEmpty then oldMean else
  var x = 0.0
  var y = 0.0
  var z = 0.0
  points.foreach { p =>
    x += p.x
    y += p.y
    z += p.z
  }
  Point(x / points.length, y / points.length, z / points.length)

val oldMeans_ = means_

def update(classified: Map[Point, Seq[Point]], oldMeans: Seq[Point]): Seq[Point] =
  oldMeans.map(oldMean => findAverage(oldMean, classified(oldMean)))


def update(classified: ParMap[Point, ParSeq[Point]], oldMeans: ParSeq[Point]): ParSeq[Point] =
  oldMeans.map(oldMean => findAverage(oldMean, classified(oldMean)))

update(classified_, oldMeans_).toList

def converged(eta: Double, oldMeans: Seq[Point], newMeans: Seq[Point]): Boolean =
  val deltaMeans = oldMeans.zip(newMeans).map((a, b) => a.squareDistance(b))
  deltaMeans.forall(_ < eta)

def converged(eta: Double, oldMeans: ParSeq[Point], newMeans: ParSeq[Point]): Boolean =
  val deltaMeans = oldMeans.zip(newMeans).map((a, b) => a.squareDistance(b))
  deltaMeans.forall(_ < eta)


@tailrec
def kMeans(points: Seq[Point], means: Seq[Point], eta: Double): Seq[Point] =
  val classifiedPoints = classify(points, means)
  val newMeans = update(classifiedPoints, means)
  val isConverged = converged(eta, means, newMeans)
  if (isConverged) then newMeans else kMeans(points, newMeans, eta)

@tailrec
def kMeans(points: ParSeq[Point], means: ParSeq[Point], eta: Double): ParSeq[Point] =
  val classifiedPoints = classify(points, means)
  val newMeans = update(classifiedPoints, means)
  val isConverged = converged(eta, means, newMeans)
  if (isConverged) then newMeans else kMeans(points, newMeans, eta)
