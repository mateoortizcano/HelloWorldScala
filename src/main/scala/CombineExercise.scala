trait Semigroup[A] {
  def combine(x:A, y: A): A
}

//usar esto con sumas u operaciones con la propiedad asociativa en el pryecto final....semigrupos y monoides
object combineOps extends App{

  implicit object combineOptions extends Semigroup[Option[Int]] {
    def combine(x: Option[Int], y: Option[Int]): Option[Int] ={
      (x, y) match {
        case (Some(a), Some(b)) => Option(combineInt.combine(a, b))
        case (a @ Some(_), _) => a
        case (_, b @ Some(_)) => b
        case _ => None
      }
    }
  }

  implicit object combineInt extends Semigroup[Int] {
    def combine(x: Int, y: Int): Int = x + y
  }

  implicit class OperateSemigroup[A] (a: A) {
    def ° (b: A)(implicit s: Semigroup[A]):  A = s.combine(a, b)
  }

  val a: Option[Int] = Some(1)
  val b: Option[Int] = None
  println(1 ° 1)
  println(a ° b)
  println(b ° a)
  println(a ° a)
  println(b ° b)
}



/*trait Semigroup[A] {
  def combine(y: A): A
}

object CombineOps extends App{

  implicit class CombineOptions[A <: Option[Int]] (x: A) extends Semigroup[Option[Int]] {
    def combine(y: Option[Int]): Option[Int] ={
      y match {
        case None => x
        case _ => for {
          val1 <- x
          val2 <- y
        } yield val1 + val2
      }
    }
  }

  implicit class CombineInt (x: Int) extends Semigroup[Int] {
    def combine(y: Int): Int = x + y
  }

  println( 1 combine 1 )
  println( Some(1) combine None )
  //println(None combine Some(1))
  println( None combine None )
  println( Some(1) combine Some(1))

}*/
