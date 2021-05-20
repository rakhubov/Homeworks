val s = List(1,22,3,444,5)
s.take(9)
s.takeRight(9)

if (s.takeRight(9) == List()) 5


s match {
  case c1 :: c2 :: c3 :: c4 :: c5 :: Nil => c1.toString + ' ' +
     c2.toString + ' ' + c3.toString + ' ' +
    c4.toString + ' ' + c5.toString + " 34 4"
  case _ => 0
}

"fd df df".split("\\s+").toList.head