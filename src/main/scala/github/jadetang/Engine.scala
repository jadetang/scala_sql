package github.jadetang

/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */


object Engine {


  import AST._
  import MetaData._

  implicit class dbTable(table: Table) {
    def from(relations: SqlRelation) = table


    def groupby(groupBy: Option[SqlGroupBy]): Seq[Table] = {

      groupBy match {
        case None => Seq(table)
        case Some(exp: SqlGroupBy) => evalGroupBy(table, exp)
      }
    }

    def where(where: Option[SqlExpr]): Table = {
      where match {
        case None => table
        case Some(x: SqlExpr) => table filter (evalWhereEachRow(_, x))
      }
    }


    def orderBy(orderBy: Option[SqlOrderBy]): Table = {
      orderBy match {
        case None => table
        case Some(x: SqlOrderBy) => evalOrderBy(table, x.keys)
      }
    }

    def limit(limit: Option[(Option[Int],Int)]   ):Table = {
      limit match {
        case None => table
        case Some(x:(Option[Int],Int))=> x match {
          case (None,i:Int)=>table.take(i)
          case (Some(offset:Int),size:Int)=> table.take(offset+size).drop(offset)
        }

      }
    }

  }

  implicit class groupByMidTable(tables: Seq[Table]) {


    def aggreFuntion(projection: Projection): Boolean = projection.sqlProj match {
      case e: SqlAgg => true
      case _ => false
    }

    def aggre(projections: Seq[Projection]): Seq[Table] = {
      if (projections exists (aggreFuntion(_))) {
        tables.map(evalAggregateFunction(_, projections))
      } else {
        tables
      }
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
      case SelectStmt(s, f, w, g, o, l) => table from f where w groupby g aggre s select s orderBy o limit l
    }
  }

  def satisfy(expr: Projection): PartialFunction[(String, MetaData[_]), (String, MetaData[_])] = {
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
            case Some(alias: String) => Map[String, MetaData[_]](alias -> evalOneAggregateFucntion(input, e).map(_._2).head)
          }
        }
        case e: FieldIdent => input.head collect {
          case (e.name, _2) => (e.name, _2)
        }
        case e: StarProj => input.head
      }
    }
    List(result.flatten.toMap)
  }


  def evalOneAggregateFucntion(input: Table, function: SqlAgg): Row = {
    function match {
      case Max(e: FieldIdent) => {
        val tuple = input.maxBy(row => row(e.name)).collect {
          case (e.name, _2) => ("max(" + e.name + ")", _2)
        }.head
        Map(tuple._1 -> tuple._2)
      }
      case Max(e: Literal) => Map("max(" + e.value + ")" -> e.value)
      case Min(e: Literal) => Map("min(" + e.value + ")" -> e.value)
      case Min(e: FieldIdent) => {
        val tuple = input.minBy(row => row(e.name)).collect {
          case (e.name, _2) => ("min(" + e.name + ")", _2)
        }.head
        Map(tuple._1 -> tuple._2)
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
          val sum = (input map (row => row(e.name))).toSet.sum
          Map("sum(distinct " + e.name + ")" -> sum)
        } else {
          val sum = (input map (row => row(e.name))).sum
          Map("sum(" + e.name + ")" -> sum)
        }
      }
      case Sum(e: Literal, distinct: Boolean) => {
        if (distinct) {
          Map("sum(distinct " + e.value + ")" -> 1)
        } else {
          e.value match {
            case e: MetaInt => Map("sum(" + e + ")" -> e.value * input.size)
            case e: MetaDouble => Map("sum(" + e + ")" -> e.value * input.size)
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
          val sum = (input map (row => row(e.name))).toSet.toList.sum
          sum match {
            case s: MetaDouble => Map("avg( distinct" + e.name + ")" -> s.value / input.size)
            case s: MetaInt => Map("avg( distinct" + e.name + ")" -> s.value / input.size)
          }
        } else {
          val sum = (input map (row => row(e.name))).toList.sum
          sum match {
            case s: MetaDouble => Map("avg(" + e.name + ")" -> s.value / input.size)
            case s: MetaInt => Map("avg(" + e.name + ")" -> s.value / input.size)
          }
        }
      }
    }
  }

  def evalWhereEachRow(row: Row, expr: SqlExpr): Boolean = {

    def eq(a: MetaData[_], b: MetaData[_]): Boolean = a == b
    def neq(a: Any, b: Any): Boolean = a != b

    def ls(a: MetaData[_], b: MetaData[_])(implicit ev1: MetaData[_] => Ordered[MetaData[_]]): Boolean
    = a < b


    def gt(a: MetaData[_], b: MetaData[_])(implicit ev1: MetaData[_] => Ordered[MetaData[_]]): Boolean
    = a > b

    def lsEq(a: MetaData[_], b: MetaData[_])(implicit ev1: MetaData[_] => Ordered[MetaData[_]]): Boolean
    = a >= b

    def gtEq(a: MetaData[_], b: MetaData[_])(implicit ev1: MetaData[_] => Ordered[MetaData[_]]): Boolean
    = a >= b



    def evalEqualityLike(sqlExpr: EqualityLike, f: (MetaData[_], MetaData[_]) => Boolean) = {
      (sqlExpr.lhs, sqlExpr.rhs) match {
        case (left: Literal, right: Literal) => f(left.value, right.value)
        case (left: FieldIdent, right: Literal) => row.get(left.name) match {
          case Some(s) => f(s, right.value)
          case None => false
        }
        case (left: FieldIdent, right: FieldIdent) => f(row(left.name), row(right.name))
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

  def evalOrderBy(table: Table, projs: Seq[SqlProj]): Table = {
    val keys: Seq[String] = projs map {
        case f: FieldIdent => f.name
    }
    table.sortBy( row=> keys.map(row(_)) )
  }


}
