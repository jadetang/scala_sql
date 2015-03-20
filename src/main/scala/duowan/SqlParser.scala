/*
package duowan

import duowan.AST.SelectColumn

import scala.util.{Failure, Success}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * Hello world!
 *
 */
object SqlParser extends StandardTokenParsers{

  lexical.reserved += ("select")

  lexical.delimiters +=("(" ,")", ";")


  lazy val selectColumn:Parser[SelectColumn] = "select"~>stringLit ^^ SelectColumn

}


*/
