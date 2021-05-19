import cats.data.OptionT
import cats.effect._
import cats.effect.concurrent.Ref
import doobie.Meta
import java.util.UUID
import scala.util.Random

val s = Set(55)
s.incl(3)


def d(h: Int, acc: Set[Int] = Set(55)):Set[Int]={
if (acc.size < h+1) d(h, acc.incl(Random.nextInt(54)))
else acc.excl(55)
}

d(5)







