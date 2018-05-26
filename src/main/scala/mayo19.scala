import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats._
import cats.data.OptionT
import cats.implicits._

class Usuario
class Id
class Direccion

//para componer con monadas porque ellas no cmponen con
object mayo19  extends App {
  def obtenerUsuario(id: Id): Future[Option[Usuario]] = Future.successful((None))
  def obtenerDirecc(idUsr: Usuario): Future[Option[Direccion]] = Future.failed(new Exception("Error"))

  def fn3(id: Id): Future[Option[Direccion]] = {
    (for {
      user <- OptionT(obtenerUsuario(id))
      direcc <- OptionT(obtenerDirecc(user))
    }yield direcc).value
  }

  case class FutOpt2[A](value: List[Option[A]]) {
    def map[B](f: A => B): FutOpt2[B] = FutOpt2[B](
      value.map(op => op.map(f))
    )

    def flatMap[B](f: A => FutOpt2[B]): FutOpt2[B] = {
      FutOpt2(
        value.flatMap( op =>
          op match {
            case Some(a) => f(a).value
            case None => List(None)
          }
        )
      )
    }
  }



  case class FutOpt[A](value: Future[Option[A]]) {
    def map[B](f: A => B): FutOpt[B] = FutOpt[B](
      value.map(op => op.map(f))
    )

    def flatMap[B](f: A => FutOpt[B]): FutOpt[B] = {
      FutOpt(
        value.flatMap( op =>
          op match {
            case Some(a) => f(a).value
            case None => Future.successful(None)
          }
        )
      )
    }
  }

}
