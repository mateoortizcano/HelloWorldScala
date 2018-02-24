import scala.annotation.tailrec

case class  A(a: Int, b:Int)


object Exec extends App {
  /*
  val aa = A(1, 2)
  aa match {
    case A(a, b) => A(a + 1, b + 1)
  }*/


  //listas
  def sumarValores(list: List[Int]): List[Int] = {
    @tailrec
    def sumarLoop(head: List[Int], tail:List[Int]): List[Int] = {
      head match {
        case h :: Nil => tail :+ (h + 1)  // que haya solo cabeza Nl es el vacio en scala
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
  println(promediarValores(List(10,5,3,2)))
  // la finalidad de la programacion funcional es preguntarse por el qué por lo tanto
  //lo anterior se define
/*val a = List(1,2,3,4,5)
a.sum / a.size*/
}


trait MensajeError {val msg: String}

case class Mensaje(texto: String, sha: Int)

object Mensaje{
  def apply(texto: String, sha: Int): Either[MensajeError, Mensaje] = {
    if(validarSha(sha)) Right(new Mensaje(texto,sha))
    else Left(new MensajeError {
      override val msg: String = "Bad sha"
    })
  }
  def validarSha(sha:Int): Boolean = ???
}