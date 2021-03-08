import FunctorAndJson._

object CodecsExercise {

  // Exercise 12. Implement Functor for `Disjunction`
  implicit def disjunctionFunctor[V]: Functor[Disjunction[V, +*]] =
    new Functor[Disjunction[V, +*]] {
      override def fmap[A, B](
                               fa: Disjunction[V, A]
                             )(f: A => B): Disjunction[V, B] =
        fa match {
          case Disjunction.Left(left)   => Disjunction.Left(left)
          case Disjunction.Right(right) => Disjunction.Right(f(right))
        }
    }


  final case class Person(name: String, age: Int)

  // Exercise 3. Implement Encoder and Decoder for Person.
  implicit val PersonEncoder: Encoder[Person] = (json: Person) =>
    JsonObject(Map(json.name.toJson -> JsonInt(json.age)))
  implicit val PersonDecoder: Decoder[Person] = (json: Json) =>
    json match {
      case JsonObject(value) =>
        for {
          (name, json) <- value.headOption
          age <- json.as[Int]
        } yield Person(name, age)
      case _ => None
    }


  // Exercise 4. Implement Encoder and Decoder for List with any content.
  implicit def listEncoder[A: Encoder]: Encoder[List[A]] =
    (json: List[A]) => JsonArray(json.map(_.toJson))
  implicit def listDecoder[A: Decoder]: Decoder[List[A]] =
    (json: Json) =>
      json match {
        case JsonArray(value) => Some(value.flatMap(_.as[A]))
        case _                => None
      }

  final case class EntityId(id: String) extends AnyVal

  // Exercise 5. Implement Encoder and Decoder for EntityId with any content.
  implicit val idEncoder: Encoder[EntityId] = (a: EntityId) => a.id.toJson
  implicit val idDecoder: Decoder[EntityId] = (json: Json) =>
    json match {
      case JsonString(value) => Some(EntityId(value))
    }

  // Exercise 6. Describe Functor
  // 1. Typeclass itself: `trait Functor`
  // 2. Typeclass Summoner: `object Functor`
  // 3. Typeclass Ops: `implicit class FunctorOps`

  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  implicit val decoderFunctor = new Functor[Decoder] {
    override def fmap[A, B](fa: Decoder[A])(f: A => B): Decoder[B] =
      new Decoder[B] {
        def fromJson(t: Json): Option[B] = fa.fromJson(t).map(f)
      }
  }

  // Exercise 8. Describe Contravariant
  // 1. Typeclass itself: `trait Contravariant`
  // 2. Typeclass Summoner: `object Contravariant`
  // 3. Typeclass Ops: `implicit class ContravariantOps`
  trait Conravariant[F[_]]{
    def contrmap[A, B](fa: F[A])(f: B => A): F[B]
  }

  object Conravariant{
    def apply[F[_]: Conravariant]: Conravariant[F] = implicitly[Conravariant[F]]
  }

  implicit class ConravariantOps[F[_]: Conravariant, A](fa: F[A]) {
    def contrmap[B](f: B => A): F[B] = Conravariant[F].contrmap(fa)(f)
  }

  // Exercise 9. Implement Contravariant for encoder: implicit val encoderContravariant
  implicit val encoderContravariant = new Conravariant[Encoder] {
    override def contrmap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = t => fa.toJson(f(t))
  }

  // Functions Example

  val fv: Functor[Boolean => *] = new Functor[Boolean => *] {
    override def fmap[A, B](fa: Boolean => A)(f: A => B): Boolean => B =
      t => f(fa(t))
  }

  val cf: Contravariant[* => Int] = new Contravariant[* => Int] {
    override def contramap[A, B](fa: A => Int)(f: B => A): B => Int =
      t => fa(f(t))
  }

  // Exercise 10. Implement Functor and Contravariant for functions:
  // implicit def functionFunctor
  // implicit def functionContravariant
  implicit def functionFunctor: Functor[Function[Boolean, *]] = fv

  implicit def functionContravariant: Contravariant[Function[*, Int]] = cf

}
