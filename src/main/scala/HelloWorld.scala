object HelloWorld extends App{
  def calcularFactorial(x: Int): Int ={
    val y: Int = if (x > 1)
      x * calcularFactorial(x-1)
    else
      1
    y
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

  def funcion1[A,B](num1: A, num2: A, x: (A, A) => B) =
    println(s"La suma de los valores es ${x(num1, num2)}")

  funcion1[Int, Int](-32, 3, suma(_,_))
}
