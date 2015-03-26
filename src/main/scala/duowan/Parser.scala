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

    /* case class Ident(chars:String) extends Token{
       override def toString = chars
     }*/

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
    elem("decimal", x => x.isInstanceOf[lexical.FloatLit]) ^^ (_.chars)

  def literal: Parser[SqlExpr] = {
    numericLit ^^ { case i => Literal(i.toInt)} |
      stringLit ^^ { case s => Literal(s.toString)} |
      floatLit ^^ { case f => Literal(f.toDouble)} |
      "null" ^^ (_ => Literal(null))
  }

  def fieldIdent: Parser[SqlExpr] = {
    ident ~ opt("." ~> ident) ^^ {
      case table ~ Some(b: String) => FieldIdent(Option(table), b)
      case column ~ None => FieldIdent(None, column)
    }
  }

  //override def ident:Parser[String] = elem("ident",x=>x.isInstanceOf[lexical.Ident]&&(!(x.chars.contains("."))))  ^^ (_.chars)
  /*
    override def ident: Parser[String] =
      elem("identifier", _.isInstanceOf[scala.util.parsing.combinator.token.Tokens.Identifier]) ^^ (_.chars)*/

  def primaryWhereExpr: Parser[SqlExpr] = {
    (literal | fieldIdent) ~ ("=" | "<>" | "!=" | "<" | "<=" | ">" | ">=") ~ (literal | fieldIdent) ^^ {
      case lhs ~ "=" ~ rhs => Eq(lhs, rhs)
      case lhs ~ "<>" ~ rhs => Neq(lhs, rhs)
      case lhs ~ "!=" ~ rhs => Neq(lhs, rhs)
      case lhs ~ "<" ~ rhs => Ls(lhs, rhs)
      case lhs ~ "<=" ~ rhs => LsEq(lhs, rhs)
      case lhs ~ ">" ~ rhs => Gt(lhs, rhs)
      case lhs ~ ">=" ~ rhs => GtEq(lhs, rhs)
    } | "(" ~> expr <~ ")"
  }

  def andExpr: Parser[SqlExpr] =
    primaryWhereExpr * ("and" ^^^ { (a: SqlExpr, b: SqlExpr) => And(a, b)})

  def orExpr: Parser[SqlExpr] =
    andExpr * ("or" ^^^ { (a: SqlExpr, b: SqlExpr) => Or(a, b)})

  def expr: Parser[SqlExpr] = orExpr

  def whereExpr: Parser[SqlExpr] = "where"~>expr

  def projectionStatements: Parser[Seq[SqlProj]] = repsep(projection, ",")

  def projection: Parser[SqlProj] =
    "*" ^^ (_ => StarProj()) |
      primarySelectExpr ~ opt("as" ~> ident) ^^ {
        case expr ~ alias => Projection(expr, alias)
      }

  def selectLiteral: Parser[SqlProj] = {
    numericLit ^^ { case i => Literal(i.toInt)} |
      stringLit ^^ { case s => Literal(s.toString)} |
      floatLit ^^ { case f => Literal(f.toDouble)} |
      "null" ^^ (_ => Literal(null))
  }


  def selectIdent: Parser[SqlProj] = {
    ident ~ opt("." ~> ident) ^^ {
      case table ~ Some(b: String) => FieldIdent(Option(table), b)
      case column ~ None => FieldIdent(None, column)
    }
  }

  def primarySelectExpr: Parser[SqlProj] = {
    selectLiteral | selectIdent | knowFunction
  }

  def knowFunction: Parser[SqlProj] = {
    def singeSelectExpr: Parser[SqlProj] = {
      selectLiteral | selectIdent
    }
    "count" ~> "(" ~> ("*" ^^ (_ => CountStar()) | opt("distinct") ~ singeSelectExpr ^^ { case d ~ e => CountExpr(e, d.isDefined)}) <~ ")" |
      "min" ~> "(" ~> singeSelectExpr <~ ")" ^^ (Min(_)) |
      "max" ~> "(" ~> singeSelectExpr <~ ")" ^^ (Max(_)) |
      "sum" ~> "(" ~> (opt("distinct") ~ singeSelectExpr) <~ ")" ^^ { case d ~ e => Sum(e, d.isDefined)} |
      "avg" ~> "(" ~> (opt("distinct") ~ singeSelectExpr) <~ ")" ^^ { case d ~ e => Avg(e, d.isDefined)}
  }

  //todo
  def select:Parser[SelectStmt] = "select" ~> projectionStatements ~ fromStatements~opt(whereExpr) ^^ {
    case p ~ f ~ w => SelectStmt(p,f,w,None,None,None)
  }

  def fromStatements:Parser[SqlRelation] = "from" ~> relations

  def parse(sql: String): Option[SelectStmt] = phrase(select)(new lexical.Scanner(sql)) match {
    case Success(r, q) => Option(r)
    case x => throw new IllegalArgumentException(x.toString);
  }

  def relations:Parser[SqlRelation] = simple_relation

  def simple_relation: Parser[SqlRelation] =ident ~ opt("as") ~ opt(ident) ^^ {
    case ident ~ _ ~ alias => TableRelationAST(ident, alias)
  }



}
