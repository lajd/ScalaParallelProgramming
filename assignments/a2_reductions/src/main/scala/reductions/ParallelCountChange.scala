package reductions

import org.scalameter.*

import scala.collection.mutable.Map

object ParallelCountChangeRunner:

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns := 20,
    Key.exec.maxWarmupRuns := 40,
    Key.exec.benchRuns := 80,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    catch
      case e: NotImplementedError =>
        println("Not implemented.")

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))

object ParallelCountChange extends ParallelCountChangeInterface:

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */

  def countChangeBaseCase(money: Int, coins: List[Int]): Int =
    if (money == 0) {
      1
    } else if (coins.isEmpty || money < 0) {
      0
    } else throw new Error("Not base case")

  def isBaseCase(money: Int, coins: List[Int]): Boolean =
    if ((money == 0) || (coins.isEmpty || money < 0)) then true else false

  def countChange(money: Int, coins: List[Int]): Int =
    if isBaseCase(money, coins)
    then countChangeBaseCase(money, coins)
    else countChange(money - coins.head, coins) +  countChange(money, coins.tail)

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int =
    if (isBaseCase(money, coins)) {
      // Early stop if base case
      countChangeBaseCase(money, coins)
    } else if (threshold(money, coins)) {
      // Sequential case when under threshold
      countChange(money, coins)
    } else {
      // Parallel case
      val par = parallel(
        parCountChange(money - coins.head, coins, threshold),
        parCountChange(money, coins.tail, threshold)
      )
      par._1 + par._2
    }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    // Return true if money is less than or equal to 2 / 3 of the starting amount:
    (money: Int, _) => money <= (startingMoney * 2.0 / 3.0)

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    //  Returns true when the number of coins is less than or equal to the 2 / 3 of the initial number of coins:
    (_, coins: List[Int]) => coins.size <= (totalCoins * 2.0 / 3.0)


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold =
    //  returns true when the amount of money multiplied with the number of remaining
    //  coins is less than or equal to the starting money multiplied with the initial
    //  number of coins divided by 2
    (money: Int, coins: List[Int]) => money * coins.size <= (startingMoney * allCoins.size / 2.0)
