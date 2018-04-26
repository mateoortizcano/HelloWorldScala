//pruebas con exresiones regulares
object ExpresionRegular {
  def evaluar(str: String): Option[String] = {
    val patron= """^([A-Z][a-z]+)( [A-Z][a-z]+)+$""".r
    patron findFirstIn str
  }
}
