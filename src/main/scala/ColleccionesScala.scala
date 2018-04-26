object Listas2 extends App {
  val frutas = "manzanas" :: ("naranjas" :: ("peras") :: Nil)
  // es igual que definirlo asi:
  val fruits = List("apples", "oranges", "pears")

  println( "Cabeza de frutas : " + frutas.head +  ", Head of fruits : " + fruits.head )
  println( "Cola de frutas : " + frutas.tail + ", Tail of fruit : " + fruits.tail )
  println( "Miremos si están vacías las frutas : " + frutas.isEmpty )


  //concatenar listas
  val nuevaLista = frutas ::: fruits
  println(nuevaLista)

  val nuevaLista2 = List.concat(fruits, frutas)
  println(nuevaLista2)

  //imprimir en reversa
  println("normal : " +  frutas + " invertida : " + frutas.reverse)

  //adcionar al inicio de la lita
  val nuevaLista3 = "grapes" :: fruits
  println(nuevaLista3)

  //agregar una lista a la lista
  val nuevaLista4 = fruits :: ("pineaples" :: Nil)
  println(nuevaLista4)

  //agregar al final de la lista
  val nuevaLista5 = fruits :+ "mangoes"
  println(nuevaLista5)

  //creando listas uniformes
  val listaUniforme = List.fill(5)("Mateo")
  println(listaUniforme)

  //lista llena con valores dada  una funcion
  val listaFuncion = List.tabulate(6)(n => n / 2)
  println(listaFuncion)

  val listaFuncion2 = List.tabulate(5,8)(_ * _)
  println(listaFuncion2)
}


object Conjuntos extends App {
  val conjunto1 : Set[Int] = Set(1, 2, 3, 4, 5, 6)
  val conjunto2 : Set[Int] = Set(4, 5, 6, 7, 8, 9, 10, 11)

  val conjuntoUnion = conjunto1 ++ conjunto2
  println(conjuntoUnion)

  val conjuntoInterseccion = conjunto1 & conjunto2
  println(conjuntoInterseccion)

  val conjuntoDiferencia1 = conjunto1 &~ conjunto2
  println(conjuntoDiferencia1)

  val conjuntoDiferencia2 = conjunto2 &~ conjunto1
  println(conjuntoDiferencia2)

}