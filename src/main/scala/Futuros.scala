import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object Futuros extends App{
  val event = Future(1) //en el lugar de 1 habŕan operaciones normalmente
  val event2 = Future {
    Thread.sleep(scala.util.Random.nextInt(50))
    //throw new Exception("Error")
    1
  } map  {_ + 1}recover {
    case ex => s"El error es ${ex.getMessage}"
  }

  //println("principal")
  //println(event.map(_ + 1))//como el hilo no se ha completado me sale error...pues el 1 aun no existe
  //val _ = Thread.sleep(2000)
  //val result = Await.result(event2, 2.seconds) //esto no se usa solo con fines de este ejemplo
  //println(event2)//como no se ha cumplido aun esta dentro del futuro
  //println("Await")
  //println(result)

println("antes")
  def int1 = Future {
    println("uno")
    1
  }
  def int2 = Future {
    println("dos")
    //2
    throw new Exception("Error 2")
  }
  def int3 = Future {
    println("tres")
    3
  }
  // los tres futuros son independientes todos ejecutados de manera independiente de manera paralela
  //CADA UNO tiene su propio timing lo que quiere decri qeu cada hilo termina en un orden diferente al
  //que son definidos
  println("antes del for")
  val result = for{
    a <- int1
    b <- int2
    c <- int3
   }yield {
    println("dentro del for")
    a + b + c
  }
  println("al final del for")
  val _ = Thread.sleep(200)
  println(result)


  //COMO HACER QUE SEAN CONCURRENTES Y NO SE EJECUTEN DE MANERA PARALELA?
  //recordemos que al definir las variables val como def o lazy val....1 2 3 se ejecutan en su orden
  //de definicion


  def getNombre: Future[Option[String]] = {
      Future(Some("Mateo"))
  }
  def getApellido: Future[Option[String]] = {
      Future(Some("Ortiz"))
  }
  def getEdad: Future[Option[String]] = {
      Future(Some("19"))
  }

  //val crear: Future[Option[String]]
}
