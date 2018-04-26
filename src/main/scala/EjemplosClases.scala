//CLASES
class Point(x: Int, y: Int) {
  override def toString(): String = s"The point is: ( $x , $y )"
}

object Classes extends App{
  override def main(args: Array[String]) {
    val pt = new Point(1, 2)
  }
}

//Options
class Movie(val name: String, val year: Short)

object Movie {
  def academyAwardBestMoviesForYear(x: Short) = {
    x match {
      case 1930 => Some(new Movie("All Quiet On the Western Front", 1930))
      case 1931 => Some(new Movie("Cimarron", 1931))
      case 1932 => Some(new Movie("Grand Hotel", 1932))
      case _ => None
    }
  }
}
//objects and tuples
class Person(val name: String, private val superheroName: String, private  val age: Int,
             private val single: Boolean)

object Person {
  def showMeInnerSecret(x: Person) = x.superheroName
  def returnPersonalInformation(x: Person) = new Tuple4(x.name, x.superheroName, x.age,x.single)
}
