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
        case Some(exp: SqlGroupBy) => evalGroupBy(table, exp)
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

    def aggre(projections: Seq[SqlProj]): Seq[Table] = {
       tables.map(evalAggregateFunction(_,projections))
    }

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
      case SelectStmt(s, f, w, g, o, l) => table from f groupby g aggre s where w select s
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
          case _ => { case(_1,_2) => (_1,_2)}
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
    val result = expr map (e => row collect (satisfy(e)))
    result.reduce(_ ++ _)
  }

  def evalSelect(input: Table, expr: Seq[SqlProj]): Table = {
    input map (selectEachRow(_, expr))
  }

  def evalAggregateFunction(input: Table, expr: Seq[SqlProj]): Table = {
    val result = expr.filter(!_.isInstanceOf[StarProj]) map{
      case Projection(e,_)=> e match {
        case e: SqlAgg => evalOneAggregateFucntion(input, e)
        case e: FieldIdent => input.head collect {
          case (e.name, _2) => (e.name, _2)
        }
      }
    }
    val x = result.flatten.toMap
    List(x)
  }

  def evalOneAggregateFucntion(input: Table, function: SqlAgg): Row = {
    def myOrder(key: String) = {
      x: Map[String, Any] =>
        x(key) match {
          case i: Int => (i, "", 0.0)
          case s: String => (0, s, 0.0)
          case d: Double => (0, "", d)
        }
    }
    function match {
      case Max(e: FieldIdent) => {
        input.maxBy(myOrder(e.name)).collect {
          case (e.name, _2) => ("max("+e.name+")", _2)
        }
      }
      case Min(e: FieldIdent) => {
        input.minBy(myOrder(e.name)).collect {
          case (e.name, _2) => ("min("+e.name+")", _2)
        }
      }
      //input.maxBy(row=>row(e.name))( f(input(0).get(e.name)) ).filter(_._1!=e.name)
      //case Min(e:FieldIdent)=>  input.minBy(row=>row(e.name)).filter(_._1!=e.name)
      //case Sum(e:FieldIdent)=>  input.(row(e.name)).filter(_._1!=e.name)
    }
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

  def evalGroupBy(table: Table, groupby: SqlGroupBy): Seq[Table] = {
    val keys: Seq[String] = groupby.keys map {
      case x: FieldIdent => x.name
    }
    table.groupBy(row => keys.map(row(_))).map(_._2).toSeq
  }

}
