case class Persona(nombre: String, edad: Int)

object Clase3Marzo extends App {
  //sumar cualquier tipo de cosa

  //implicit
  def sum(a:Int)(implicit b: Int): Int = a + b
  def sum(a: String, b: String): String = a + b
  //si escriben dos valores en el mismo scope los valores serian implicitos existe ambiguedad
  implicit val v= 456
  implicit  def tostr : Int => String = _.toString
  //com no concuerdan los valores, entonces el compilador busca una funcion que permita hacer la conversion
  //al tipo requerido
  println(sum("string ",2))
  println(sum(3)) //como no pase el segundo parametro lo asume como 99

  implicit def personToInt: Persona => Int = _.edad

  def addEdad(edad: Int, valor: Int): Int = edad + valor
  val persona = new Persona("Mateo", 20)
  println(addEdad(persona, 21))

}


