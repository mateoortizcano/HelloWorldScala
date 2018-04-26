import scala.annotation.tailrec

case class  A(a: Int, b:Int)

object Listas {
  val aa = A(1, 2)
  aa match {
    case A(a, b) => A(a + 1, b + 1)
  }
  //listas y recursividad
  def sumarUnoValores(list: List[Int]): List[Int] = {
    @tailrec
    def sumarLoop(head: List[Int], tail:List[Int]): List[Int] = {
      head match {
        case h :: Nil => tail :+ (h + 1)  // que haya solo cabeza Nil es el vacio en scala
        case h :: t => sumarLoop(t, tail :+ (h+1)) //que haya cabeza y cola
        case _ => tail
      }
    }
    sumarLoop(list,Nil)
  }

  def promediarValores(list: List[Int]): Double = {
    @tailrec
    def sumarLoop(head: List[Int], acum:Double, nroElem:Double): Double = {
      head match {
        case h :: Nil => (acum+h)/nroElem // que haya solo cabeza Nl es el vacio en scala
        case h :: t => sumarLoop(t,h+acum,nroElem+1) //que haya cabeza y cola
        case Nil => acum/nroElem
      }
    }
    sumarLoop(list,0D,1D)
  }
}






//no tiene pruebas
trait MensajeError {val msg: String}
trait estado
trait cifrado extends estado
trait plano extends estado

case class Mensaje[S <: estado](texto: String, sha: Int)

object Mensaje{
  def apply(texto: String, sha: Int): Either[MensajeError, Mensaje[plano]] = {
    if(validarSha(sha)) Right(new Mensaje[plano](texto,sha))
    else Left(new MensajeError {
      override val msg: String = "Bad sha"
    })
  }
  def validarSha(sha:Int): Boolean = ???
}