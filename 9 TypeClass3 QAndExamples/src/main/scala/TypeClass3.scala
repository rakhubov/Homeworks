object QAndAExamples {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
  }

  implicit class SemigroupSyntax[A](x: A)(implicit j: Semigroup[A]) {
    def combine(y: A): A = j.combine(x, y)
  }


  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]]{
    def product[A,B](fa: F[A], fb: F[B]): F[(A, B)]
  }
  object  Semigroupal{
    def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly
  }

  implicit class SemSyn[F[_]: Semigroupal ,A](fa: F[A]){
    def product[B](fb: F[B]): F[(A,B)] = Semigroupal[F].product(fa, fb)
  }


  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  implicit  class MapNOption[F[_]: Functor : Semigroupal, A, B](x: (F[A], F[B])){
    def mapN[R](f: (A, B) => R): F[R] = x match {
      case (fa, fb) =>fa.product(fb).fmap(f.tupled) //   ( case(a,b) => f(a,b))
    }
  }
  (Option(1), Option("sds")).mapN(_ + _.length)





  // 4.4. Implement Semigroupal for Map

  implicit def semigroupalMap[T]: Semigroupal[Map[T, *]] = new Semigroupal[Map[T, *]] {
    override def product[A, B](fa: Map[T, A], fb: Map[T, B]): Map[T, (A, B)] = {for{
      a <- fa
      bv <- fb.get(a._1)
    }yield (a._1 -> (a._2, bv))
    }
  }
  implicit  def fonctorMap[T]: Functor[Map[T, *]] = new Functor[Map[T, *]] {
    override def fmap[A, B](fa: Map[T, A])(f: A => B): Map[T, B] = fa.view.mapValues(f).toMap
  }

  (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  object Applicative{
    def apply[F[_]](implicit inctance: Applicative[F]): Applicative[F] = inctance
  }
  implicit case class ApplicativeSyntax[F[_]: Applicative, A](x: A){
    def pure: F[A] = Applicative[F].pure(x)
  }

  // 5.1. Implement Applicative for Option, Either
  implicit  val applicativeEither: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)
    override def product[A, B](
                                fa: Either[String, A],
                                fb: Either[String, B]
                              ): Either[String, (A, B)] = (fa, fb) match {
      case (Right(a), Right(b)) => Right(a,b)
      case (Left(a), _)           => Left(a)
      case (_, Left(b))           => Left(b)
      case _ => Left("All is Left")
    }
    override def fmap[A, B](fa: Either[String, A])(
      f: A => B
    ): Either[String, B] = fa match {
      case Right(b) => Right(f(b))
      case Left(a) => Left(a)

    }
  }

  implicit  val applicativeOption: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def product[A, B](
                                fa: Option[A],
                                fb: Option[B]
                              ): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(a,b)
      case (_ , _)         => None

    }
    override def fmap[A, B](fa: Option[A])(
      f: A => B
    ): Option[B] = fa match {
      case Some(b) => Some(f(b))
      case _ => None

    }
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    Some(as.map(f(_)).filter(_ == Some ).flatMap(x => x))

  traverse(List(1, 2, 3)) { i =>
    Option.when(i % 2 == 1)(i)
  } == None

  traverse(List(1, 2, 3)) { i =>
    Some(i + 1)
  } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def foldLeftN[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  }

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library
  implicit def foldLeftFoldable: Foldable[List] = new Foldable[List] {
    override def foldLeftN[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }

  // 6.3. Implement `traverse` for all Foldables instead of List
}
