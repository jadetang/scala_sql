package github.jadetang

import MetaData.MetaData


object AST {

  trait Node

  trait SqlExpr extends Node

  trait Binop extends SqlExpr {
    val lhs: SqlExpr
    val rhs: SqlExpr
  }

  case class Literal(value: MetaData[_]) extends SqlExpr with SqlProj

  case class FieldIdent(qualify: Option[String], name: String) extends SqlExpr with SqlProj

  case class Or(lhs: SqlExpr, rhs: SqlExpr) extends Binop

  case class And(lhs: SqlExpr, rhs: SqlExpr) extends Binop

  trait EqualityLike extends Binop

  case class Eq(lhs: SqlExpr, rhs: SqlExpr) extends EqualityLike

  case class Neq(lhs: SqlExpr, rhs: SqlExpr) extends EqualityLike

  case class Ls(lhs: SqlExpr, rhs: SqlExpr) extends EqualityLike

  case class Gt(lhs: SqlExpr, rhs: SqlExpr) extends EqualityLike

  case class LsEq(lhs: SqlExpr, rhs: SqlExpr) extends EqualityLike

  case class GtEq(lhs: SqlExpr, rhs: SqlExpr) extends EqualityLike

  sealed trait SqlProj extends Node

  trait SqlAgg extends Node

  case class StarProj() extends SqlProj

  case class Projection(sqlProj: SqlProj, alias: Option[String]) extends Node

  case class CountStar() extends SqlProj with SqlAgg

  case class CountExpr(expr: SqlProj, distinct: Boolean) extends SqlProj with SqlAgg

  case class Sum(expr: SqlProj, distinct: Boolean) extends SqlProj with SqlAgg

  case class Avg(expr: SqlProj, distinct: Boolean) extends SqlProj with SqlAgg

  case class Min(expr: SqlProj) extends SqlProj with SqlAgg

  case class Max(expr: SqlProj) extends SqlProj with SqlAgg

  case class SelectStmt(projections: Seq[Projection],
                        relations: SqlRelation,
                        where: Option[SqlExpr],
                        groupBy: Option[SqlGroupBy],
                        orderBy: Option[SqlOrderBy],
                        limit: Option[(Option[Int],Int)]) extends Node

  trait SqlRelation extends Node

  case class TableRelationAST(name: String, alias: Option[String]) extends SqlRelation

  case class SqlGroupBy(keys:Seq[SqlProj]) extends Node

  case class SqlOrderBy(keys:Seq[SqlProj]) extends Node

}
