package Homework4

object Solution {
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)(_ + _).tail
  }
}

object shuffl {
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val start = nums.take(n)
    val end = nums.drop(n)
    (start zip end).flatMap { case (a, b) => Array(a, b) }
  }
}
