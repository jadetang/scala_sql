package duowan

import duowan.Parser._
import duowan.AST._

/**
 * Created by jadetang on 15-3-22.
 */
class ParserTest extends org.specs2.mutable.Specification {

  "int literal parser" in {
    val number = "30"
    val result = literal(new  Parser.lexical.Scanner(number))
    log(result)
    val sql = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value should equalTo(30)
  }


  "string literal parser" in {
    val sex = """ 'male' """
    val result = literal(new Parser.lexical.Scanner(sex))
    log(result)
    val sql  = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value should equalTo("male")
  }

  "null parser" in {
    val nullexpress = """ null """
    val result = literal(new Parser.lexical.Scanner(nullexpress))
    log(result)
    val sql  = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value mustEqual null
  }

  "double value" in {
    val doublevalue = "3.2"
    val result = literal(new Parser.lexical.Scanner(doublevalue))
    log(result)
    val sql  = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value mustEqual 3.2
  }



  def log(result:ParseResult[SqlExpr]) = {
    result match {
      case Success(msg, _) => println(msg)
      case Failure(msg, _) => println("Failure: " + msg)
      case Error(msg, _) => println("Error: " + msg)
    }
  }

}
