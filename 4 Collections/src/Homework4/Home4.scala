package Homework4

object Runsum {
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).tail
  }
}

object Shuffl {
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val start = nums.take(n)
    val end = nums.drop(n)
    (start zip end).flatMap { case (a, b) => Array(a, b) }
  }
}

object Maximum {
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.flatMap { case a => Array(a.sum) }.max
  }
}

object KidCandies {
  def kidsWithCandies(
      candies: Array[Int],
      extraCandies: Int
  ): Array[Boolean] = {
    candies.map(_ + extraCandies >= candies.max)
  }
}

object Maxwidth {
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val xs = points.map(_.head).sorted
    xs.zip(xs.tail).flatMap { case (a, b) => Array(b - a) }.max
  }
}
