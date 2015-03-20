package duowan


/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
object Engine {
  type Row = Map[String, Any]
  type Table = List[Row]

  import AST._

  def evalWhere(input: Table, expr: SqlExpr): Table = {
    input filter (evalWhereEachRow(_, expr))
  }


  def evalWhereEachRow(row: Row, expr: SqlExpr): Boolean = {

    def eq(a: Any, b: Any): Boolean = a == b
    def neq(a: Any, b: Any): Boolean = a != b

    // def ls[T](a:T,b:T)(implicit evl: T => Ordered[T]):Boolean =  a < b

   //  def gt[T](a:T,b:T)(implicit evl: T => Ordered[T]):Boolean =  a > b

  //   def lsEq[T](a:T,b:T)(implicit evl: T => Ordered[T]):Boolean = a <= b

  //   def gtEq[T](a:T,b:T)(implicit evl: T => Ordered[T]):Boolean = a >= b*/

    def ls(a: Any, b: Any): Boolean = {
      a match {
        case (a: Comparable[Any]) => (a compareTo b) < 0
        case _ => false
      }
    }

    def gt(a:Any, b:Any):Boolean = {
      a match {
        case (a: Comparable[Any]) => (a compareTo b) > 0
        case _ => false
      }
    }

    def lsEq(a:Any, b:Any):Boolean = {
      a match {
        case (a: Comparable[Any]) => (a compareTo b) <= 0
        case _ => false
      }
    }

    def gtEq(a:Any, b:Any):Boolean = {
      a match {
        case (a: Comparable[Any]) => (a compareTo b) >= 0
        case _ => false
      }
    }


    def evalEqualityLike(sqlExpr: EqualityLike, f: (Any, Any) => Boolean) = {
      (sqlExpr.lhs, sqlExpr.rhs) match {
        case (left: Literal, right: Literal) => f(left.value, right.value)
        case (left: FieldIdent, right: Literal) => row.get(left.name) match {
          case Some(s) => f(s, right.value)
          case None => false
        }
      }
    }

    expr match {
      case And(left, right) => evalWhereEachRow(row, left) && evalWhereEachRow(row, right)
      case Or(left, right) => evalWhereEachRow(row, left) || evalWhereEachRow(row, right)
      case (expr: Eq) => evalEqualityLike(expr, eq)
      case (expr: Neq) => evalEqualityLike(expr, neq)
      case (expr: Ls) => evalEqualityLike(expr, ls)
      case (expr: Gt) => evalEqualityLike(expr, gt)
      case (expr: LsEq) => evalEqualityLike(expr, lsEq)
      case (expr: GtEq) => evalEqualityLike(expr, gtEq)
    }
  }

}
