package duowan

import duowan.Parser._
import duowan.AST._

/**
 * Created by jadetang on 15-3-22.
 */
class ParserTest extends org.specs2.mutable.Specification {

  "int literal parser" in {
    val number = "30"
    val result = literal(new Parser.lexical.Scanner(number))
    log(result)
    val sql = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value should equalTo(30)
  }


  "string literal parser" in {
    val sex = """ 'male' """
    val result = literal(new Parser.lexical.Scanner(sex))
    log(result)
    val sql = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value should equalTo("male")
  }

  "null parser" in {
    val nullexpress = """ null """
    val result = literal(new Parser.lexical.Scanner(nullexpress))
    log(result)
    val sql = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value mustEqual null
  }

  "double value" in {
    val doublevalue = "3.2"
    val result = literal(new Parser.lexical.Scanner(doublevalue))
    log(result)
    val sql = result.get
    sql.isInstanceOf[Literal] should beTrue
    sql.asInstanceOf[Literal].value mustEqual 3.2
  }


  "table name and column" in {
    val table = "user"
    val column = "age"
    val str = "user.age"
    val result = fieldIdent(new lexical.Scanner(str))
    log(result)
    val sql = result.get
    sql.isInstanceOf[FieldIdent] should beTrue
    sql.asInstanceOf[FieldIdent].qualify mustEqual table
    sql.asInstanceOf[FieldIdent].name mustEqual column

  }

  " (user.age = 30)  " in {
    val str = " user.age = 30 "
    val result = primaryWhereExpr(new lexical.Scanner(str))
    log(result)
    val sql = result.get
    sql.isInstanceOf[Eq] should beTrue
    sql.asInstanceOf[Eq].lhs.isInstanceOf[FieldIdent] should beTrue
    sql.asInstanceOf[Eq].rhs.isInstanceOf[Literal] should beTrue
  }

  "where user.age = 30 and (user.name = 'tsc' and sex = 'male') " in {
    val str = """ where user.age = 30 and (user.name = 'tsc' and sex='male') """
    val result = whereExpr(new lexical.Scanner(str))
    log(result)
    val sql = result.get
    sql.isInstanceOf[And] should beTrue
  }

  /* " user.age = 30 and (user.name = 'tsc' or user.sex= 'male') " in {
     val str = """ where user.age = 30 and user.name = 'tsc' and user.sex= 'male'  """
     val result = whereExpr(new lexical.Scanner(str))
     log(result)
     val sql = result.get
     sql.isInstanceOf[And] should beTrue
   }*/

  "select test" >> {
    "select all kind" in {
      val str = """ select user.age as age, '1',1,2.2, max(name), count(distinct name), sum(age), avg(name)  """
      val result = projectionStatements(new lexical.Scanner(str))
      logSelect(result)
      //log(result)
      val sql = result.get
      sql.isInstanceOf[Seq[SqlProj]] should beTrue
    }

    "select name as g" in {
      val str = """ select name as 2 """
      phrase(projectionStatements)(new lexical.Scanner(str)) match {
        case Success(r, q) => Option(r)
        case x => println(x); None
      }
      val result = projectionStatements(new lexical.Scanner(str))
      //logSelect(result)
      //log(result)
      val sql = result.get
      sql.isInstanceOf[Seq[SqlProj]] should beTrue
    }



  }


  def logSelect(result: Parser.ParseResult[Seq[SqlProj]]): Unit = {
    result match {
      case Success(msg, _) => println(msg)
      case Failure(msg, _) => println("Failure: " + msg)
      case Error(msg, _) => println("Error: " + msg)
    }
  }

  def log(result: ParseResult[SqlExpr]) = {
    result match {
      case Success(msg, _) => println(msg)
      case Failure(msg, _) => println("Failure: " + msg)
      case Error(msg, _) => println("Error: " + msg)
    }
  }

}
