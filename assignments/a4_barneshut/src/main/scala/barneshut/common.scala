package barneshut

import java.util.concurrent.*
import scala.{collection => coll}
import scala.util.DynamicVariable
import barneshut.conctrees.*

class Boundaries:
  var minX = Float.MaxValue

  var minY = Float.MaxValue

  var maxX = Float.MinValue

  var maxY = Float.MinValue

  def width = maxX - minX

  def height = maxY - minY

  def size = math.max(width, height)

  def centerX = minX + width / 2

  def centerY = minY + height / 2

  override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"

trait Quad extends QuadInterface:
  def massX: Float

  def massY: Float

  def mass: Float

  def centerX: Float

  def centerY: Float

  def size: Float

  def total: Int

  def insert(b: Body): Quad

  def spatiallyContains(b: Body): Boolean =
  // Returns true if this quad spatially contains the body
    if (b.x > (centerX - size / 2.0)
      && b.x < (centerX + size / 2.0)
      && b.y > (centerY - size / 2.0)
      && b.y < (centerY + size / 2.0)) then true else false


case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad:
  def massX: Float = centerX
  def massY: Float = centerY
  def mass: Float = 0f
  def total: Int = 0

  def insert(b: Body): Quad =
    // Insert a leaf node representing the body
    Leaf(
      centerX,
      centerY,
      size,
      Seq(b)
    )

case class Fork(
  nw: Quad, ne: Quad, sw: Quad, se: Quad
) extends Quad:
  val forkQuads = List(nw, ne, se, sw)
  // Geometric center of fork is the geometric center of all quads
  val centerX: Float = forkQuads.map(_.centerX).sum / 4
  val centerY: Float = forkQuads.map(_.centerY).sum / 4
  // Since all quads have save size, fork size is 2x any quad size
  val size: Float = nw.size * 2
  // Mass is sum of masses of quads
  val mass: Float = forkQuads.foldLeft(0f)((sum, b) => sum + b.mass)
  val total: Int = forkQuads.foldLeft(0)((sum, b) => sum + b.total)

  // Mean of quad massX
  val massX: Float =
    if (total == 0f) centerX
    else forkQuads.foldLeft(0f)((sum, quad) =>  sum + quad.mass * quad.massX) / mass

  // Mean of quad massY
  val massY: Float =
    if (total == 0f) centerY
    else forkQuads.foldLeft(0f)((sum, quad) =>  sum + quad.mass * quad.massY) / mass

  override def spatiallyContains(b: Body): Boolean =
    forkQuads.map(q => q.spatiallyContains(b)).contains(true)

  def insert(b: Body): Fork = b match {
    case c1 if (nw.spatiallyContains(b)) => Fork(nw.insert(b), ne, sw, se)
    case c2 if (ne.spatiallyContains(b)) => Fork(nw, ne.insert(b), sw, se)
    case c3 if (sw.spatiallyContains(b)) => Fork(nw, ne, sw.insert(b), se)
    case c4 if (se.spatiallyContains(b)) => Fork(nw, ne, sw, se.insert(b))
  }

case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: coll.Seq[Body]) extends Quad:
  val mass = bodies.foldLeft(0f)((sum, b) => sum + b.mass)
  val massX = bodies.foldLeft(0f)((sum, body) =>  sum + body.mass * body.x) / mass
  val massY = bodies.foldLeft(0f)((sum, body) =>  sum + body.mass * body.y) / mass
  //
  val total: Int = bodies.length

  def insert(b: Body): Quad =
    val newBodies = bodies :+ b
    if (size <= minimumSize)
      Leaf(centerX, centerY, size, newBodies)
    else
      // We fork the current quad into 4 subquads
      //   -- --
      //  |  |  | (quadDim)
      //  -- o --
      //  |  |  |
      //   -- --
      //
      val quadSize = size / 2
      val quadHalfSize = size / 4
      val (leftX, rightX) = (centerX - quadHalfSize, centerX + quadHalfSize)
      val (topY, bottomY) = (centerY - quadHalfSize, centerY + quadHalfSize)
      // Define new quads
      val nw = Empty(leftX, topY, quadSize)
      val ne = Empty(rightX, topY, quadSize)
      val sw = Empty(leftX, bottomY, quadSize)
      val se = Empty(rightX, bottomY, quadSize)

      val fork = Fork(nw, ne, sw, se)
      // Insert all the bodies into the fork
      newBodies.foreach(fork.insert)
      fork






def minimumSize = 0.00001f

def gee: Float = 100.0f

def delta: Float = 0.01f

def theta = 0.5f

def eliminationThreshold = 0.5f

def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float =
  math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat

class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float):

  def updated(quad: Quad): Body =
    var netforcex = 0.0f
    var netforcey = 0.0f

    def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit =
      val dist = distance(thatMassX, thatMassY, x, y)
      /* If the distance is smaller than 1f, we enter the realm of close
        * body interactions. Since we do not model them in this simplistic
        * implementation, bodies at extreme proximities get a huge acceleration,
        * and are catapulted from each other's gravitational pull at extreme
        * velocities (something like this:
        * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
        * To decrease the effect of this gravitational slingshot, as a very
        * simple approximation, we ignore gravity at extreme proximities.
        */
      if dist > 1f then
        val dforce = force(mass, thatMass, dist)
        val xn = (thatMassX - x) / dist
        val yn = (thatMassY - y) / dist
        val dforcex = dforce * xn
        val dforcey = dforce * yn
        netforcex += dforcex
        netforcey += dforcey

    def traverse(quad: Quad): Unit = (quad: Quad) match
      case Empty(_, _, _) =>
        // no force
      case Leaf(_, _, _, bodies) => bodies.foreach(b =>
        addForce(b.mass, b.x, b.y)
      )
        // add force contribution of each body by calling addForce
      case Fork(nw, ne, sw, se) =>
        val dist = distance(quad.massX, quad.massY, x, y)
        val singlePointApprox = (quad.size / dist) < theta
        // see if node is far enough from the body,
        // or recursion is needed
        if (singlePointApprox) {
          // Approxiamte as single point
          addForce(quad.mass, quad.massX, quad.massY)
        } else {
          List(nw, ne, sw, se).foreach(traverse)
        }


    traverse(quad)

    val nx = x + xspeed * delta
    val ny = y + yspeed * delta
    val nxspeed = xspeed + netforcex / mass * delta
    val nyspeed = yspeed + netforcey / mass * delta

    Body(mass, nx, ny, nxspeed, nyspeed)


val SECTOR_PRECISION = 8

class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) extends SectorMatrixInterface:
  val sectorSize = boundaries.size / sectorPrecision

  def newSector =
    new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)

  val matrix = newSector
  for i <- 0 until matrix.length do matrix(i) = ConcBuffer()

  def sectorCoordsFromBody(b: Body): (Int, Int) =
    // The body can be outside of the bounds, in which case
    // we find the closest sector.
    val clampedX = Math.min(Math.max(b.x, boundaries.minX), boundaries.maxX)
    val clampedY = Math.min(Math.max(b.y, boundaries.minY), boundaries.maxY)
    ((clampedX / sectorSize).floor.toInt, (clampedY / sectorSize).floor.toInt)

  def +=(b: Body): SectorMatrix =
    val (sectorX, sectorY) = sectorCoordsFromBody(b)
    this(sectorX, sectorY) += b
    this


  def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

  def combine(that: SectorMatrix): SectorMatrix =
    // Combine sector matrices
    // This method assumes sectors of same dimension, same precision
    for (i <- 0 until matrix.length)
      this.matrix(i) = this.matrix(i).combine(that.matrix(i))
    this

  def toQuad(parallelism: Int): Quad =
    def BALANCING_FACTOR = 4
    def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad =
      if span == 1 then
        val sectorSize = boundaries.size / sectorPrecision
        val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
        val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
        var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
        val sectorBodies = this(x, y)
        sectorBodies.foldLeft(emptyQuad)(_ insert _)
      else
        val nspan = span / 2
        val nAchievedParallelism = achievedParallelism * 4
        val (nw, ne, sw, se) =
          if parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR then parallel(
            quad(x, y, nspan, nAchievedParallelism),
            quad(x + nspan, y, nspan, nAchievedParallelism),
            quad(x, y + nspan, nspan, nAchievedParallelism),
            quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
          ) else (
            quad(x, y, nspan, nAchievedParallelism),
            quad(x + nspan, y, nspan, nAchievedParallelism),
            quad(x, y + nspan, nspan, nAchievedParallelism),
            quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
          )
        Fork(nw, ne, sw, se)

    quad(0, 0, sectorPrecision, 1)

  override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"

class TimeStatistics:
  private val timeMap = collection.mutable.Map[String, (Double, Int)]()

  def clear() = timeMap.clear()

  def timed[T](title: String)(body: =>T): T =
    var res: T = null.asInstanceOf[T]
    val totalTime = /*measure*/
      val startTime = System.currentTimeMillis()
      res = body
      (System.currentTimeMillis() - startTime)

    timeMap.get(title) match
      case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
      case None => timeMap(title) = (totalTime.toDouble, 1)

    println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
    res

  override def toString =
    timeMap map {
      case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
    } mkString("\n")

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
