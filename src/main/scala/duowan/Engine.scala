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

    def aggre(projections: Seq[Projection]): Seq[Table] = {
      tables.map(evalAggregateFunction(_, projections))
    }

    def select(projections: Seq[Projection]): Table = {
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


  def satisfy(expr: Projection): PartialFunction[(String, Any), (String, Any)] = {
    (expr.sqlProj, expr.alias) match {
      case (StarProj(), _) => {
        case (_1, _2) => (_1, _2)
      }
      case (f: FieldIdent, alias) => alias match {
        case None => {
          case (f.name, _2) => (f.name, _2)
        }
        case Some(alias: String) => {
          case (f.name, _2) => (alias, _2)
        }
      }
      case (l: Literal, alias) => alias match {
        case None => {
          case (_1, _2) => (l.value.toString, l.value)
        }
        case Some(alias: String) => {
          case (_1, _2) => (alias, l.value)
        }
      }
      case (l: SqlAgg, alias) => {
        case (_1, _2) => (_1, _2)
      }
    }
  }

  def selectEachRow(row: Row, expr: Seq[Projection]): Row = {
    val result = expr map (e => row collect (satisfy(e)))
    result.reduce(_ ++ _)
  }

  def evalSelect(input: Table, expr: Seq[Projection]): Table = {
    input map (selectEachRow(_, expr))
  }

  def evalAggregateFunction(input: Table, expr: Seq[Projection]): Table = {
    val result = expr map {
      case Projection(e, alias) => e match {
        case e: SqlAgg => {
          alias match {
            case None => evalOneAggregateFucntion(input, e)
            case Some(alias: String) => Map[String, Any](alias -> evalOneAggregateFucntion(input, e).map(_._2).head)
          }
        }
        case e: FieldIdent => input.head collect {
          case (e.name, _2) => (e.name, _2)
        }
        case e: StarProj => input.head
      }
    }
    List(result.flatten.toMap)
    // List(x)
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
    def sum(list: List[Any]): Any = {
      list match {
        case l: List[Int] => l.sum
        case l: List[Double] => l.sum
      }
    }
    function match {
      case Max(e: FieldIdent) => {
        input.maxBy(myOrder(e.name)).collect {
          case (e.name, _2) => ("max(" + e.name + ")", _2)
        }
      }
      case Max(e: Literal) => Map("max(" + e.value + ")" -> e.value)
      case Min(e: Literal) => Map("min(" + e.value + ")" -> e.value)
      case Min(e: FieldIdent) => {
        input.minBy(myOrder(e.name)).collect {
          case (e.name, _2) => ("min(" + e.name + ")", _2)
        }
      }
      case CountStar() => {
        Map("count(*)" -> input.size)
      }
      case CountExpr(e: FieldIdent, distinct: Boolean) => {
        if (distinct) {
          val countDist = (input map (row => row(e.name)) toSet).size
          Map("count(distinct " + e.name + ")" -> countDist)
        } else {
          Map("count(distinct " + e.name + ")" -> input.size)
        }
      }
      case CountExpr(e: Literal, distinct: Boolean) => {
        if (distinct) {
          Map("count(distinct" + e.value + ")" -> 1)
        } else {
          Map("count(distinct" + e.value + ")" -> input.size)

        }
      }
      case Sum(e: FieldIdent, distinct: Boolean) => {
        if (distinct) {
          val valueSet = (input map (row => row(e.name))).toSet.toList
          Map("sum(distinct " + e.name + ")" -> sum(valueSet))
        } else {
          val valueBag = (input map (row => row(e.name)))
          Map("sum(" + e.name + ")" -> sum(valueBag))
        }
      }
      case Sum(e: Literal, distinct: Boolean) => {
        if (distinct) {
          Map("sum(distinct " + e.value + ")" -> 1)
        } else {
          val value = e.value
          value match {
            case e: Int => Map("sum(" + e + ")" -> e * input.size)
            case e: Double => Map("sum(" + e + ")" -> e * input.size)
            case _ => Map("sum(" + e + ")" -> input.size)
          }
        }
      }
      case Avg(e: Literal, distinct: Boolean) => {
        if (distinct) {
          Map("avg(distinct " + e.value + ")" -> e.value)
        } else {
          Map("avg(" + e.value + ")" -> e.value)
        }
      }
      case Avg(e: FieldIdent, distinct: Boolean) => {
        if (distinct) {
          val valueSet = (input map (row => row(e.name))).toSet.toList
          sum(valueSet) match {
            case sum: Double => Map("avg(distinct " + e.name + ")" -> sum / input.size)
            case sum: Int => Map("avg(distinct " + e.name + ")" -> sum / input.size)
          }
        } else {
          val valueList = (input map (row => row(e.name))).toList
          sum(valueList) match {
            case sum: Double => Map("avg(" + e.name + ")" -> sum / input.size)
            case sum: Int => Map("avg(" + e.name + ")" -> sum / input.size)
          }
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
