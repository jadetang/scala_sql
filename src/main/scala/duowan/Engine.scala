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
  type Schema = Map[String,Table]
  type DataBase = Map[String,Schema]

  import AST._

  def evalWhere(input: Table, expr: SqlExpr): Table = {
    input filter (evalWhereEachRow(_, expr))
  }

  //todo
  def satisfy(expr: SqlProj): PartialFunction[(String, Any), (String,Any)] = {
    /*expr match {
      case (x:StarExpr)=> {case(_1,_2)=>(_1,_2)}
      case (x:FieldIdent)=>{case(x.name,_2)=>(x.name,_2)}
    }*/
    ???
  }

  def selectEachRow(row: Row, expr: SqlProj): Row = {
    row collect(satisfy(expr))
  }

  def evalSelect(input:Table, expr: SqlProj):Table = {
    input map(selectEachRow(_,expr))
  }


  def evalWhereEachRow(row: Row, expr: SqlExpr): Boolean = {

    def eq(a: Any, b: Any): Boolean = a == b
    def neq(a: Any, b: Any): Boolean = a != b

    def ls(a: Any, b: Any): Boolean = {
      if (a.getClass == b.getClass) {
        a match {
          case (a: Comparable[Any]) => (a compareTo b) < 0
          case _ => false
        }
      } else {
        false
      }
    }

    def gt(a: Any, b: Any): Boolean = {
      if (a.getClass == b.getClass) {
        a match {
          case (a: Comparable[Any]) => (a compareTo b) > 0
          case _ => false
        }
      } else {
        false
      }
    }

    def lsEq(a: Any, b: Any): Boolean = {
      if (a.getClass == b.getClass) {
        a match {
          case (a: Comparable[Any]) => (a compareTo b) <= 0
          case _ => false
        }
      } else {
        false
      }
    }

    def gtEq(a: Any, b: Any): Boolean = {
      if (a.getClass == b.getClass) {
        a match {
          case (a: Comparable[Any]) => (a compareTo b) >= 0
          case _ => false
        }
      } else {
        false
      }
    }



    def evalEqualityLike(sqlExpr: EqualityLike, f: (Any, Any) => Boolean) = {
      (sqlExpr.lhs, sqlExpr.rhs) match {
        case (left: Literal, right: Literal) => f(left.value, right.value)
        case (left: FieldIdent, right: Literal) => row.get(left.name) match {
          case Some(s) => f(s, right.value)
          case None => false
        }
        case (left: FieldIdent, right: FieldIdent) => f(row.get(left.name),row.get(right.name))
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
