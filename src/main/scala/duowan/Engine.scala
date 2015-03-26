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
  type Schema = Map[String, Table]
  type DataBase = Map[String, Schema]


  import duowan.AST._

  implicit class dbTable(table: Table) {
    def from(relations: SqlRelation) = table

    def groupby(groupBy: Option[SqlGroupBy]) = {
      groupBy match {
        case None => Seq(table)
        case _ => ??? //TODO
      }
    }

  }

  implicit class groupByMidTable(tables: Seq[Table]) {
    def where(where: Option[SqlExpr]): Seq[Table] = {
      where match {
        case None => tables
        case Some(x: SqlExpr) => tables map (evalWhere(_, x))
      }
    }

    def aggregate = tables

    def select(projections: Seq[SqlProj]): Table = {
      tables.map(evalSelect(_, projections)) reduce (_ ++ _)
    }

  }


  def query(input: Table, sql: String): Table = {
    val ast = Parser.parse(sql)
    ast match {
      case Some(r: SelectStmt) => execute(input, r)
    }
  }

  def execute(table: Table, sql: SelectStmt): Table = {
    sql match {
      case SelectStmt( s , f , w , g ,o ,l) => table from f groupby g where w select s
    }
  }


  def evalWhere(input: Table, expr: SqlExpr): Table = {
    input filter (evalWhereEachRow(_, expr))
  }


  def satisfy(expr: SqlProj): PartialFunction[(String, Any), (String, Any)] = {
    expr match {
      case StarProj() => {
        case (_1, _2) => (_1, _2)
      }
      case Projection(sqlProj, alias) => {
        sqlProj match {
          case (f: FieldIdent) => alias match {
            case None => {
              case (f.name, _2) => (f.name, _2)
            }
            case Some(alias: String) => {
              case (f.name, _2) => (alias, _2)
            }
          }
          case (l: Literal) => alias match {
            case None => {
              case (_1, _2) => (l.value.toString, l.value)
            }
            case Some(alias: String) => {
              case (_1, _2) => (alias, l.value)
            }
          }
        }
      }
      /*case (FieldIdent(qualify, name)) => {
        qualify match {
          case None =>{case (name, _2) => (name, _2)}
          case Some(b: )
        }
      }*/
    }
  }

  def selectEachRow(row: Row, expr: Seq[SqlProj]): Row = {
    //(expr map (e => row collect (satisfy(e)))).reduce(_ ++ _)
    val result = expr map (e => row collect (satisfy(e)))
    //println(result)
    result.reduce(_ ++ _)
  }

  def evalSelect(input: Table, expr: Seq[SqlProj]): Table = {
    input map (selectEachRow(_, expr))
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
        case (left: FieldIdent, right: FieldIdent) => f(row.get(left.name), row.get(right.name))
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
