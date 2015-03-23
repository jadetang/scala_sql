package duowan


import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
object Parser extends StandardTokenParsers {

  import AST._

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

  def literal: Parser[SqlExpr] = {
    numericLit ^^ { case i => Literal(i.toDouble) } |
    stringLit ^^ { case s => Literal(s) } |
    "null" ^^ (_ => Literal(null))
  }

  /* def filter: Parser[SqlExpr] = "where" ~> expr

   def expr: Parser[SqlExpr] = or_expr

   def or_expr: Parser[SqlExpr] =
     and_expr * ( "or" ^^^ { (a: SqlExpr, b: SqlExpr) => Or(a, b) } )

   def and_expr: Parser[SqlExpr] =
     cmp_expr * ( "and" ^^^ { (a: SqlExpr, b: SqlExpr) => And(a, b) } )

   def cmp_expr: Parser[SqlExpr] =
     add_expr ~ rep(
       ("=" | "<>" | "!=" | "<" | "<=" | ">" | ">=") ~ add_expr ^^ {
         case op ~ rhs => (op, rhs)
       } |
         "between" ~ add_expr ~ "and" ~ add_expr ^^ {
           case op ~ a ~ _ ~ b => (op, a, b)
         } |
         opt("not") ~ "in" ~ "(" ~ (select | rep1sep(expr, ",")) ~ ")" ^^ {
           case n ~ op ~ _ ~ a ~ _ => (op, a, n.isDefined)
         } |
         opt("not") ~ "like" ~ add_expr ^^ { case n ~ op ~ a => (op, a, n.isDefined) }
     ) ^^ {
       case lhs ~ elems =>
         elems.foldLeft(lhs) {
           case (acc, (("=", rhs: SqlExpr))) => Eq(acc, rhs)
           case (acc, (("<>", rhs: SqlExpr))) => Neq(acc, rhs)
           case (acc, (("!=", rhs: SqlExpr))) => Neq(acc, rhs)
           case (acc, (("<", rhs: SqlExpr))) => Lt(acc, rhs)
           case (acc, (("<=", rhs: SqlExpr))) => Le(acc, rhs)
           case (acc, ((">", rhs: SqlExpr))) => Gt(acc, rhs)
           case (acc, ((">=", rhs: SqlExpr))) => Ge(acc, rhs)
           case (acc, (("between", l: SqlExpr, r: SqlExpr))) => And(Ge(acc, l), Le(acc, r))
           case (acc, (("in", e: Seq[_], n: Boolean))) => In(acc, e.asInstanceOf[Seq[SqlExpr]], n)
           case (acc, (("in", s: SelectStmt, n: Boolean))) => In(acc, Seq(Subselect(s)), n)
           case (acc, (("like", e: SqlExpr, n: Boolean))) => Like(acc, e, n)
         }
     } |
       "not" ~> cmp_expr ^^ (Not(_)) |
       "exists" ~> "(" ~> select <~ ")" ^^ { case s => Exists(Subselect(s)) }
 */


}
