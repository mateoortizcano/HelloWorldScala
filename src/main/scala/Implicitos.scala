sealed trait Persona {val nombre: String; val edad: Int; val patrimonio: Int}
case class Juridica(val nombre: String, val edad: Int, val patrimonio: Int) extends Persona


object Implicitos extends App {
  //sumar cualquier tipo de cosa

  //implicit
  def sum(a:Int)(implicit b: Int): Int = a + b
  def sum(a: String, b: String): String = a + b

  //si escriben dos valores en el mismo scope los valores serian implicitos existe ambiguedad
  implicit val v= 456

  //com no concuerdan los valores, entonces el compilador busca una funcion que permita hacer la conversion
  //al tipo requerido
  //no es recomendable hacer transformaciones implicitas, no es correcto
  implicit  def tostr : Int => String = _.toString

  println(sum("string ",2))
  println(sum(3)) //como no pase el segundo parametro lo asume como 456

  implicit def personToInt: Persona => Int = _.edad

  def addEdad(edad: Int, valor: Int): Int = edad + valor
  val persona = new Juridica("Mateo", 20, 2)
  println(addEdad(persona, 21))

  def func(a: String, b: String): Boolean = {
    a.length >= b.length
  }
  println(func("Mateo", "Cano"))

  //las clases implicitas se usan para dar extensibilidad, es decir que da funciones nuevas a tipos
  //ya existentes
  implicit class Operacion(s: String){
    def >==(r: String): Boolean = s.length >= r.length
  }

  println("Mateo" >=="Cano")
  //es igual a "Mateo".>==("Cano")

}

//problema: sumar cualquier tipo de cosa
//ME PERMITE TENER POLIMORFISMO Y ADEMAS ME OFRECE UN OPEN/CLOSE
 trait Sumable[T] {
   def sumar(a: T, b: T): T
   def zero: T
 }

object  SumableOps extends App{
  implicit object Sumable extends Sumable[Int] {
    def sumar(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }

  implicit object StringSumable extends Sumable[String] {
    def sumar(a: String, b: String): String = a + b
    def zero: String = ""
  }

  implicit object PersonaSumable extends Sumable[Persona] {
    def sumar(a: Persona, b: Persona): Persona = Juridica("Mateo", 21, a.patrimonio + b.patrimonio)
    def zero: Persona = Juridica("Mateo", 21, 0)
  }

  //evitar hacer esto
  this.StringSumable.sumar("a","b")
  //en su lugar hacer esto
  //los parametros de la funcion deben ser curreados
  def sum [T : Sumable](a: T, b: T)(implicit s: Sumable[T]):  T = s.sumar(a, b)
  println(sum(1, 2))
  println(sum("Mateo ", "Ortiz"))
}



