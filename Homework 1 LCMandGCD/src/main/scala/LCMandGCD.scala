object LCMandGCD extends App {

  def lcm(a: Int, b: Int): Option[Int] = {
    if (a == 0 || b == 0) None
    else Some(Math.abs(b * (a / gcd(a, b))))
  }

  def gcd(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) Math.abs(a + b)
    else if (Math.abs(a) < Math.abs(b)) gcd(b, a)
    else gcd(a % b, b)
  }

}
