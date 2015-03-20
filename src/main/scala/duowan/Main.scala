/*
// Listing 8.4 Running the DSL Processor

package duowan

object Main {

  def main(args: Array[String]) {
    val str = """select "column" """"
    import SqlParser._
    selectColumn(new lexical.Scanner(str)) match {
      case Success(order, _) => println(order)
      case Failure(msg, _) => println("Failure: " + msg)
      case Error(msg, _) => println("Error: " + msg)
    }
  }
}
*/
