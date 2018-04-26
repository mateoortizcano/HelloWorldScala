import  scala.annotation.tailrec

object Funciones extends App{
  def calcularFactorial(x: Int): Int ={
    val y: Int = if (x > 1)
      x * calcularFactorial(x-1)
    else
      1
    y
  }

  //HACIENDO USO DE FUNCIONES PARCIALES
  def calcularFactorial2(x: Int): Int = x match{
    case 0 => 1
    case _ => x * calcularFactorial2(x - 1)
  }

  def fact0: Int => Int = {case 0 => 1}
  def fact1: Int => Int = {case n => n * fact1(n-1)}
  //def fact: Int => Int = fact0 orElse fact1


  //soluciona los problemas de stackOverFlow, pues los llamados no se apilan
  def calcularFactorial3(x: Int): Int = {
    @tailrec
    def factLoop(n: Int, acum: Int): Int = {
      if (n <= 0) acum else factLoop(n - 1, n * acum)
    }
    factLoop(x, 1)
  }

  def calcularFibonacci(nroValores: Int, nro1: Int = 0, nro2: Int = 1): Int = {
    val y: Int = if(nroValores > 1)
      calcularFibonacci(nroValores-1, nro2, nro1+nro2)
    else if (nroValores == 1)
      nro1+nro2
    else 0
    y
  }
  def suma(x: Int, y: Int): Int = x + y

  //funcion deo orden superior
  def funcion1[A,B](num1: A, num2: A, x: (A, A) => B) =
    println(s"La suma de los valores es ${x(num1, num2)}")

  funcion1[Int, Int](-32, 3, suma)

  //FUNCIONES IMPLEMENTADAS PARCIALMENTE
  val y : Int => Int = suma(3,_)
  println(y(2))


  //como definir una funcion de otra manera
  def div : Int=> Int => Int = x => y => y + x


  //MCD
  def calcularMDC1(x :Int, y:Int): Int = {
    val menor: Int = math.min(x, y)
    @tailrec
    def calcularMCD(x: Int, y: Int, ind: Int): Int = {
      if (y % menor == 0 && x % menor == 0) menor else calcularMCD(x, y, menor-1)
    }
    calcularMCD(x, y, menor)
  }
  println(calcularMDC1(96, 17))

  //MCD2
  @tailrec
  def GCD(a: Int, b: Int): Int = if(b == 0) a else GCD(b, a&b)
}

//funciones de orden superior
object FuncionAnonima {

  //COMO DEFINIR funciones anonimas (expresiones lambda)
  def lambda = { x: Int =>
    x + 1
  }
  def lambda2 = (x: Int) ⇒ x + 2
  val lambda3 = (x: Int) ⇒ x + 3

  val lambda4 = new Function1[Int, Int] {
    def apply(v1: Int): Int = v1 - 1
  }

  def lambda5(x: Int) = x + 1
}


object FuncionOrdenSuperior {
  //recibe una funcion como parametro
  def sumador(x: Int, y: Int => Int) = y(x)
  def suma = (x: Int) ⇒ x + 5

  //retorna una funcion
  def funcion(x: Int): Function1[Int, Int] = {
    new Function1[Int, Int]() {
      def apply(y: Int): Int = x + y
    }
  }
  //el metodo anterior se puede escribir como funcion anonima así:
  def funcion2(x: Int) = (y: Int) ⇒ x + y
}







