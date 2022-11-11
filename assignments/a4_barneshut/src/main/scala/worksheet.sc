import barneshut._


case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad:
  def massX: Float = centerX
  def massY: Float = centerY
  def mass: Float = 0
  def total: Int = 0
  def insert(b: Body): Quad = ???