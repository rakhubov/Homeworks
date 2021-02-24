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
  User("1", "Oleg").show(user => user.id)

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
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method
}
