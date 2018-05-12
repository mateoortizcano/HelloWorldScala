import scala.util.{Success, Try}

trait Monada[F[_]] {
  def pure[A]: A => F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}


object MonadaOps {
  implicit object MonadaOption extends Monada[Option] {
    override def pure[A]: A => Option[A] = Option.apply

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f
  }

  type either[A] = Either[String, A]

  implicit  object  MonadaEither extends Monada[either] {
    override def pure[A]: A => either[A] = Right.apply

    override def flatMap[A, B](fa: either[A])(f: A => either[B]): either[B] =
      fa flatMap f
  }

  implicit object MonadaTry extends Monada[Try] {
    override def pure[A]: A => Try[A] = Success.apply

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa flatMap f
  }


  trait MonadLaws {
    def f[A, F[_]]: A => F[A]
    def g[A, F[_]]: A => F[A]

    def leftIdentity[A, F[_]: Monada](x: A)(f: A => F[A])(m: Monada[F]): Boolean = {
      m.flatMap(m.pure(x))(f) == f(x) == f(x)
    }

    def rightIdentity[A, F[_]: Monada](x: A)(m: Monada[F]): Boolean = {
      m.flatMap(m.pure(x))(m.pure) == m.pure
    }

    def associativityIdentity[A, F[_]: Monada](x: A)(m: Monada[F])(f: A => F[A])(g: A => F[A]): Boolean =
    {
      val ap = m.pure(x)
      m.flatMap(m.pure(ap))(a => m.flatMap(f(x))(g)) == m.flatMap(m.flatMap(ap)(f))(g)
    }
  }
}
