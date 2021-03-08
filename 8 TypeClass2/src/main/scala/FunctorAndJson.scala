object FunctorAndJson {

  //Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }
  // 2. Syntax helper
  implicit class FunctorOps[F[_], A](fa: F[A])(implicit functor: Functor[F]) {
    def fmap[B](f: A => B): F[B] = implicitly[Functor[F]].fmap(fa)(f)
  }
  //Summoner
  object Functor {
    def apply[F[_]](implicit functor: Functor[F]): Functor[F] = functor
  }

  sealed trait Disjunction[+A, +B]
  object Disjunction {
    case class Left[+A, +B](left: A) extends Disjunction[A, B]
    case class Right[+A, +B](right: B) extends Disjunction[A, B]
  }


  //Json
  sealed trait Json {

    def /(key: String): JsonResult =
      this match {
        case JsonObject(value) => JsonResult(value.get(key))
        case _                 => JsonResult(None)
      }
  }
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  final case class JsonResult(v: Option[Json]) {
    def as[A: Decoder]: Option[A] =
      v.flatMap(Decoder[A].fromJson)
  }

  trait Contravariant[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  // Encoder
  trait Encoder[A] {
    def toJson(a: A): Json
  }

  object Encoder {
    def apply[A: Encoder]: Encoder[A] = implicitly[Encoder[A]]
  }

  implicit class EncoderOps[A: Encoder](a: A) {
    def toJson: Json = Encoder[A].toJson(a)
  }

  // Decoder
  trait Decoder[A] { self =>
    def fromJson(json: Json): Option[A]
    def fmap[B](f: A => B): Decoder[B] =
      (json: Json) => self.fromJson(json).map(f)
  }

  object Decoder {
    def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]
  }

  implicit class DecoderOps(json: Json) {
    def as[A: Decoder]: Option[A] = Decoder[A].fromJson(json)
  }





  // Exercise 1. Implement Encoder and Decoder for Int.
  implicit val IntEncoder: Encoder[Int] = (a: Int) => JsonInt(a)
  implicit val IntDecoder: Decoder[Int] = (json: Json) =>
    json match {
      case JsonInt(value) => Some(value)
      case _              => None
    }

  // Exercise 2. Implement Encoder and Decoder for String.
  implicit val StringEncoder: Encoder[String] = (a: String) => JsonString(a)
  implicit val StringDecoder: Decoder[String] = (json: Json) =>
    json match {
      case JsonString(value) => Some(value)
      case _                 => None
    }

}


