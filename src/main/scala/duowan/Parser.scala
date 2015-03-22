package duowan

import duowan.AST._

import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader.EofCh


/**
 * Created by jadetang on 15-3-22.
 */
object Parser extends StandardTokenParsers {


  class SqlLexical extends StdLexical {

    case class FloatLit(chars: String) extends Token {
      override def toString = chars
    }

    override def token: Parser[Token] =
      (identChar ~ rep(identChar | digit) ^^ { case first ~ rest => processIdent(first :: rest mkString "")}
        | rep1(digit) ~ opt('.' ~> rep(digit)) ^^ {
        case i ~ None => NumericLit(i mkString "")
        case i ~ Some(d) => FloatLit(i.mkString("") + "." + d.mkString(""))
      }
        | '\'' ~ rep(chrExcept('\'', '\n', EofCh)) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "")}
        | '\"' ~ rep(chrExcept('\"', '\n', EofCh)) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "")}
        | EofCh ^^^ EOF
        | '\'' ~> failure("unclosed string literal")
        | '\"' ~> failure("unclosed string literal")
        | delim
        | failure("illegal character")
        )

    def regex(r: Regex): Parser[String] = new Parser[String] {
      def apply(in: Input) = {
        val source = in.source
        val offset = in.offset
        val start = offset // handleWhiteSpace(source, offset)
        (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
          case Some(matched) =>
            Success(source.subSequence(start, start + matched.end).toString,
              in.drop(start + matched.end - offset))
          case None =>
            Success("", in)
        }
      }
    }
  }

  override val lexical = new SqlLexical
  val functions = Seq("count", "sum", "avg", "min", "max", "substring", "extract")

  lexical.reserved +=(
    "select", "as", "or", "and", "group", "order", "by", "where", "limit",
    "join", "asc", "desc", "from", "on", "not", "having", "distinct",
    "case", "when", "then", "else", "end", "for", "from", "exists", "between", "like", "in",
    "year", "month", "day", "null", "is", "date", "interval", "group", "order",
    "date", "left", "right", "outer", "inner"
    )

  lexical.reserved ++= functions

  lexical.delimiters +=(
    "*", "+", "-", "<", "=", "<>", "!=", "<=", ">=", ">", "/", "(", ")", ",", ".", ";"
    )
  def floatLit: Parser[String] =
    elem("decimal", _.isInstanceOf[lexical.FloatLit]) ^^ (_.chars)

  def literal: Parser[SqlExpr] = {
    numericLit ^^ { case i => Literal(i.toInt)} |
      stringLit ^^ { case s => Literal(s.toString)} |
      floatLit ^^ { case f => Literal(f.toDouble)} |
    "null" ^^ (_ => Literal(null))
  }

  //def primaryWhereExpr: Parser[SqlExpr] =


}
