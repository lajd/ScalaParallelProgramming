package scalashop

import java.util.concurrent.*
import scala.util.Random
import scala.collection.*

class BlurSuite extends munit.FunSuite:
  val rand = new scala.util.Random

  def deterministicPixel(i: Int, j: Int) = (Math.pow(i + j, 2) % 255).toInt
  def deterministicRGBA(i: Int, j: Int) =
    rgba(
      deterministicPixel(i, j), deterministicPixel(i + 1, j),
      deterministicPixel(i + 1, j + 1), deterministicPixel(i, j + 1)
    )

  def deterministicImage(n: Int): Img =
    val src = new Img(n, n)
    for (i <- 0 until n; j <- 0 until n)
      src(i, j) = deterministicRGBA(i, j)  // Random value
    src

  def constantImage(n: Int, c: Int): Img =
    val src = new Img(n, n)
    for (i <- 0 until n; j <- 0 until n)
      src(i, j) = c  // Random value
    src

  trait TestImages {
    val detImg = deterministicImage(10)
    val constImage = constantImage(10, 5)

    val testImg = new Img(3, 3)
      testImg(0, 0) = 0; testImg(1, 0) = 3; testImg(2, 0) = 6
      testImg(0, 1) = 1; testImg(1, 1) = 4; testImg(2, 1) = 7
      testImg(0, 2) = 2; testImg(1, 2) = 5; testImg(2, 2) = 8
  }

  // Put tests here
  test("Find neighbours within radius - no clamp - 1") {
    val neibs = neighbouringPixels(0, 0, 10, 10, 1)
    assertEquals(neibs, List((0,0), (0,1), (1,0), (1,1)))
  }

  test("Find neighbours within radius - no clamp - 2") {
    val neibs = neighbouringPixels(5, 5, 10, 10, 2)
    assertEquals(
      neibs,
      List(
        (3,3), (3,4), (3,5), (3,6), (3,7),
        (4,3), (4,4), (4,5), (4,6), (4,7),
        (5,3), (5,4), (5,5), (5,6), (5,7),
        (6,3), (6,4), (6,5), (6,6), (6,7),
        (7,3), (7,4), (7,5), (7,6), (7,7)
      )
    )
  }

  test("Find neighbours within radius - with clamp - 3") {
    val neibs = neighbouringPixels(8, 8, 10, 10, 4)
    assertEquals(
      neibs,
      List(
        (4,4), (4,5), (4,6), (4,7), (4,8), (4,9),
        (5,4), (5,5), (5,6), (5,7), (5,8), (5,9),
        (6,4), (6,5), (6,6), (6,7), (6,8), (6,9),
        (7,4), (7,5), (7,6), (7,7), (7,8), (7,9),
        (8,4), (8,5), (8,6), (8,7), (8,8), (8,9),
        (9,4), (9,5), (9,6), (9,7), (9,8), (9,9))
    )
  }

  test("Deterministic Image Validation") {
    new TestImages:
      for (i <- 0 until detImg.width; j <- 0 until detImg.height)
        assertEquals(detImg(i, j), deterministicRGBA(i, j))
  }

  test("boxBlurKernel with radius 0 is identity") {
    new TestImages:
      for (i <- 0 until detImg.width; j <- 0 until detImg.height)
        assertEquals(boxBlurKernel(detImg, i, j, 0), detImg(i, j))
  }

  test("boxBlurKernel on constant image of any radius is identity") {
    new TestImages:
      for (i <- 0 until constImage.width; j <- 0 until constImage.height)
        assertEquals(boxBlurKernel(constImage, i, j, 5), constImage(i, j))
  }

  test("boxBlurKernel returns correct value on edge") {
    new TestImages:
      assert(boxBlurKernel(testImg, 0, 0, 1) == (0 + 1 + 3 + 4) / 4)
  }

  test("boxBlurKernel returns correct value within interior") {
    new TestImages :
      assert(boxBlurKernel(testImg, 1, 1, 1) == (Range(0, 9).sum) / 9)
  }


  test("VerticalBoxBlur.blur returns correct value on edge") {
    new TestImages:
      val dst = new Img(3, 3)
      VerticalBoxBlur.blur(testImg, dst, 0, 3, 1)
      assert(dst(1, 1) == (Range(0, 9).sum) / 9)
  }

  test("HorizontalBoxBlue.blur returns correct value within interior") {
    new TestImages:
      val dst = new Img(3, 3)
      HorizontalBoxBlur.blur(testImg, dst, 0, 3, 1)
      assert(dst(1, 1) == (Range(0, 9).sum) / 9)
  }

  test("VerticalBoxBlue.parBlur returns correct value within interior") {
    new TestImages:
      val dst = new Img(3, 3)
      VerticalBoxBlur.parBlur(testImg, dst, 3, 1)
      assertEquals(dst(1, 1), (Range(0, 9).sum) / 9)
  }

  test("HorizontalBoxBlur.parBlur returns correct value within interior") {
    new TestImages:
      val dst = new Img(3, 3)
      HorizontalBoxBlur.parBlur(testImg, dst, 3, 1)
      assertEquals(dst(1, 1), (Range(0, 9).sum) / 9)
  }


