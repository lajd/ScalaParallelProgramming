package reductions

import java.util.concurrent.*
import scala.collection.*
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory
import scala.collection.mutable.Stack

class ReductionsSuite extends munit.FunSuite:

  trait TestChars:
    val longLength = 1000000
    val longChars = new Array[Char](longLength)
    val longThreshold = 10000

    longChars(50) = '('
    longChars(500) = ')'
    longChars(5000) = '('
    longChars(50000) = ')'
    longChars(500000) = '('


/*****************
   * LINE OF SIGHT *
   *****************/

  import LineOfSight.*
  test("lineOfSight should correctly handle an array of size 4 with threshold 2") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assertEquals(output.toList, List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight should correctly handle an array of size 4 with threshold 2") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 2)
    assertEquals(output.toList, List(0f, 1f, 4f, 4f))
  }

  test("upsweep should work with threshold 1") {
    val output = new Array[Float](4)
    val x = upsweep(Array[Float](0f, 1f, 8f, 9f), 0, 4, 1)
    println(x)
  }

  test("sequential and parallel LoS should return the same value for input of size 4 and any threshold") {
    val output1 = new Array[Float](4)
    val output2 = new Array[Float](4)
    val output3 = new Array[Float](4)
    val output4 = new Array[Float](4)

    val input = Array(0f, 1f, 8f, 9f)
    lineOfSight(input, output1)

    parLineOfSight(input, output2, 2)
    parLineOfSight(input, output3, 1)
    parLineOfSight(input, output4, 4)

    assert(output1.toList == output2.toList)
    assert(output2.toList == output3.toList)
    assert(output3.toList == output4.toList)
  }



  /*******************************
   * PARALLEL COUNT CHANGE SUITE *
   *******************************/

  import ParallelCountChange.*

  test("countChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) =
      assert(countChange(money, coins) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange should return 1 when money == 0") {
    def check(coins: List[Int]) =
      assert(countChange(0, coins) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) =
      assert(countChange(money, List()) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("countChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }


  /**********************************
   * PARALLEL PARENTHESES BALANCING *
   **********************************/

  import ParallelParenthesesBalancing.*

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("parBalance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("parBalance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 1) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("parBalance should work for long strings") {
    new TestChars:
      val segmentStacks = ParallelParenthesesBalancing._parTraverse(longChars, longThreshold, 0, longChars.length)
      val reducedStacks= ParallelParenthesesBalancing._reduceStacks(segmentStacks)
      assert(reducedStacks == Stack('('))
  }



  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

