package kmeans


import java.util.concurrent.*
import scala.collection.mutable.Stack
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.parallel.CollectionConverters.*
import scala.math.*


class KMeansSuite extends munit.FunSuite:

  object KM extends KMeans
  import KM.*

  trait Tester:

    val points_ = List(
      Point(1, 2, 3),
      Point(2, 4, 5),
      Point(6, 7, 8),
      Point(7, 10, 9),
      Point(8, 7, 7),
      Point(10, 10, 13),
      Point(12, 9, 12),
      Point(15, 11, 11)
    )

    val distinctPoints_ = List(
      Point(0, 0, 0),
      Point(5, 5, 5),
      Point(10, 10, 10),
    )

    val means_ = List(
      Point(1, 1, 1),
      Point(6, 8, 7),
      Point(10, 9, 11),
    )

  def checkParClassify(points: ParSeq[Point], means: ParSeq[Point], expected: ParMap[Point, ParSeq[Point]]): Unit =
    assertEquals(classify(points, means), expected, s"classify($points, $means) should equal to $expected")

  test("'classify' should work for empty 'points' and empty 'means'") {
    val points: ParSeq[Point] = IndexedSeq().par
    val means: ParSeq[Point] = IndexedSeq().par
    val expected = ParMap[Point, ParSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test("'classify' should work for test points and initial means") {
    new Tester:
      val classifiedPoints = classify(points_.par, means_.par)
      assertEquals(classifiedPoints.size, means_.length)
  }

  test("'classify' should work for same test points as means") {
    new Tester:
      val classifiedPoints = classify(distinctPoints_.par, distinctPoints_.par)
      assertEquals(
        classifiedPoints.map((k, v) => k).seq.toSet,
        distinctPoints_.toSet
      )
  }

  test("'update' should work for test points and initial means") {
    new Tester:
      val classifiedPoints = classify(points_.par, means_.par)
      val newMeans = update(classifiedPoints, means_.par)
      assertEquals(newMeans.size, means_.length)
  }

  test("'update' means should be in the same order as original means") {
    new Tester:
      val classifiedPoints = classify(means_.par, means_.par)
      val newMeans = update(classifiedPoints, means_.par)
      assertEquals(newMeans.seq.mkString(","), means_.mkString(","))
  }


  test("'converge' on equal means should return true") {
      new Tester:
        assertEquals(
          converged(1e-3, means_.par, means_.par),
          true
        )
  }

  test("'converge' on different means should return false") {
    new Tester:
      val diffMeans_ = List(
        Point(9, 1, 1),
        Point(6, 8, 7),
        Point(10, 9, 11),
      )

      assertEquals(
        converged(1e-3, means_.par, diffMeans_.par),
        false
      )
  }

  test("'kMeans' for distinct points are distinct points") {
    new Tester:
      val newMeans = kMeans(distinctPoints_.par, means_.par, 1e-5)
      assertEquals(newMeans.seq.mkString(","), distinctPoints_.mkString(","))
  }

  test("'kMeans' returns expected results") {
    new Tester:
      val pts = List(Point(0, 0, 1), Point(0,0, -1), Point(0,1,0), Point(0,10,0))
      val mns = List(Point(0, -1, 0), Point(0, 2, 0))
      val newMeans = kMeans(pts.par, mns.par, 12.25)
      assertEquals(newMeans.seq.toList.mkString(","), List(Point(0.0, 0.0, 0.0), Point(0.0, 5.5, 0.0)).mkString(","))
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
