import java.util.concurrent.*
import scala.util.DynamicVariable
import org.scalameter.*

import scalashop._

type RGBA = Int
def red(c: RGBA): Int = (0xff000000 & c) >>> 24
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0
def rgbaToChannelValues(rgba: RGBA): List[Int] =
  List(red(rgba), green(rgba), blue(rgba), alpha(rgba))

def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

// Test image
val photoCanvas = new PhotoCanvas()

println("width: " + photoCanvas.image.width)
println("height: " + photoCanvas.image.height)

/** Find neighbouring pixels within radius of source pixel */
def neighbouringPixels(x: Int, y: Int, w: Int, h: Int, radius: Int): List[(Int, Int)] =
  // Only return pixels which are within bounds
  val neighbours = for
    i <- -radius to radius
    j <- -radius to radius
  yield (clamp(x + i, 0, w - 1), clamp(y + j, 0, h - 1))
  // Note that due to clamping, neighbours may not be unique
  neighbours.distinct.toList

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA =
  val neighbours = neighbouringPixels(x, y, src.width, src.height, radius)
  val rgbaArr = neighbours
    .map((i, j) => rgbaToChannelValues(src(i, j)))
    .foldLeft(Array(0, 0, 0, 0))(
      (i, j) => i.zip(j).map((i,j) => i + j)
    ).map(x => x / 4)
  rgba(rgbaArr(0), rgbaArr(1), rgbaArr(2), rgbaArr(3))


// Testing
val radius = 3
val width = 1920
val height = 1080
val src = new Img(width, height)
val dst = new Img(width, height)

val verticallyBlurredSequential = VerticalBoxBlur.blur(src, dst, 0, width, radius)
val horizontallyBlurredSequential = HorizontalBoxBlur.blur(src, dst, 0, width, radius)

VerticalBoxBlur.parBlur(src, dst, width, radius)
HorizontalBoxBlur.parBlur(src, dst, width, radius)
