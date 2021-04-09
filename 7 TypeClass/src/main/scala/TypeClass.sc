object TypeclassTask {

  // Why am I not a Typeclass?
  // TODO: Rework me so I am a typeclass
  trait HashCode[A] {
    def hash(hachCode : A): Int
  }

  // TODO: Implement me a summoner
  object HashCode {
  def apply[F](implicit instance: HashCode[F]): HashCode[F] = instance
  }

  def printHashCode[A: HashCode](x: A): Unit = {
    println(HashCode[A].hash(x))
  }

  // TODO: Implement syntax so I can "abc".hash
  object HashSyntax {

    implicit class HashCodeSyntax[A](x: A) {
def hash(implicit instance: HashCode[A]):Int ={
        instance.hash(x)
      }
    }
    //import HashSyntax._
    def printHashCode[A: HashCode](x:A): Unit = {
      println(x.hash)
    }
  }

  // TODO: make an instance for String
  implicit val stringtoInt: HashCode[String] =
    _.hashCode

  // TODO: write "abc".hash to check everything
  import HashSyntax._
  "abc".hash

}

object Task1 {
  final case class Money(amount: BigDecimal)

  // TODO: create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  // TODO: create Show instance for User
  // TODO: create syntax for Show so i can do User("1", "Oleg").show
  implicit val showUser: Show[User] = _.name

  implicit class ShowUser[T](entity: T) {
    def show(implicit shw: Show[T]): String = shw.show(entity)
  }

  def show[T : Show](sh: T): String = sh.show

  User("1", "Oleg").show
  show(User("1", "Oleg"))

}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)
  // TODO: create Parse instance for User
  // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)
  implicit val parseUser: Parse[User] = str => str.split(",").toList match {
    case id :: name :: Nil => Right(User(id, name))
    case _ => Left("Error string")
  }

  implicit class ParseString(x: String) {
    def parse[A](implicit parser: Parse[A]): Either[Error, A] = parser.parse(x)
  }

  "lalala".parse[User]

}

object Task4 {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`

  implicit class EqSyntax[T](x: T) {
    def ===(typ: T): Boolean = x == typ
  }

  3 === 5
  // 3 === "s"
}