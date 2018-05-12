trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] =
    implicitly[Functor[F]]


  implicit def Tuple2Functor[A1]: Functor[({type f[x] = (A1, x)})#f] =
    new Functor[({type f[x] = (A1, x)})#f] {
      def map[A, B](a: (A1, A))(f: A => B): (A1, B) = (a._1, f(a._2))
  }

  implicit def Tuple2Functor2[A1]: Functor[({type f[x] = (x, A1)})#f] =
    new Functor[({type f[x] = (x, A1)})#f] {
      def map[A, B](a: (A, A1))(f: A => B): (B, A1) = (f(a._1),(a._2))
    }

}

