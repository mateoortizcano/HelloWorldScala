//Varianza y Contravarianza, mapas y Options ()
sealed abstract class Vehículo
class Carro extends Vehículo
class Moto extends Vehículo
object DemoVarianzaContravarianza {
  class Parqueadero[+A](movil: A)
  //esto es covarianza
  val parqueadero1: Parqueadero[Vehículo] = new Parqueadero[Carro](new Carro)
  //al final como Vehiculo >: Carro entonces Parqueadero[Hehiculo] >: Parqueadero[Carro]
  //esto es contravarianza
  class Parqueadero2[-A]
  val parqueadero2: Parqueadero2[Moto] = new Parqueadero2[Vehículo]
  //al final como Moto <: Vehiculo entonces Parquedero2[Moto] <: Parqueadero2[Vehiculo]
}

//OPTION: lidear con ausencia de valores
object OptionDemo extends App {
  val capitales = Map("Francia" -> "Paris", "Japon" -> "Tokyo", "Colombia" -> "Bogotá", "Argentina" -> "Buenos Aires")
  println("capitales.get( \"Francia\" ) : " +  capitales.get( "Francia" ))
  println("capitales.get( \"India\" ) : " +  capitales.get( "Rusia" ))
  val s: Option[String] = capitales.get( "India" )
  //dentro del gerOrElse pongo el valor que quiero sacar si el option retorna un None
  println(s.getOrElse("No reconozco el pais"))

  def mostrarMensajeCompleto(capital: String): String = {
    capital+"?"
  }
  //como manda s directo al parametro capital?
  println(s.fold("No esta registrado ese país")(mostrarMensajeCompleto))

  //para operar valores que estén dentro de un option podemos hacer esto
  val capital1: Option[String] = capitales.get( "Colombia" )
  val capital2: Option[String] = capitales.get( "Japon" )

  val salida: Option[String]= for {
    cap1 <- capital1
    cap2 <- capital2
  } yield s"$cap1 es un la capital de un pais ubicado al sur de America pero $cap2 no lo es."
  //pero corro el riesgo de que alguna de las variables sea None
  println(salida.get) //aqui saldría error
}