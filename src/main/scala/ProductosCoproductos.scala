class ProductosCoproductos {
  // un producto es un conjunto de atributos que componen una clase
  //esto es un coproducto, es decir polimorfismo. una moneda puede ser dolar o euro o peso
  sealed trait Currency {val value: Double}
  case class US(value: Double) extends Currency
  case class EUR(value: Double) extends  Currency
  case class COP(value: Double) extends Currency

  object objeto1 {
    def toCOP(c: Currency, trms: List[(Currency, Double)],
              f: Currency => Double => List[(Currency, Double)] => Double): Double = {
      c match {
        case usd @ US(v) => f(usd) (v) (trms)
        case eur @ EUR(v) => f(eur) (v) (trms)
        case cop @ COP(v) => f (cop) (v) (trms)
      }
    }

    def funcion (currency: Currency) (v: Double) (trms: List[(Currency, Double)]): Double = {
      v * trms.find {
        case (usd, trm) => true
      }.get._2
    }
  }
}
