package duowan


import duowan.AST._
import duowan.Parser._
import duowan.TestData._
import duowan.Engine._

/**
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
class EngineTest extends org.specs2.mutable.Specification {

  val nameEqTsc: SqlExpr = Eq(FieldIdent(null, "name"), Literal("tsc"))
  val alwaysTrue: SqlExpr = Eq(Literal("1"), Literal("1"))
  val undefinedColumn: SqlExpr = Eq(FieldIdent(null, "job"), Literal("driver"))
  val sexEqNull: SqlExpr = Eq(FieldIdent(null, "sex"), Literal(null))
  val nameNeqTsc: SqlExpr = Neq(FieldIdent(null, "name"), Literal("tsc"))
  val sexNeqNull = Neq(FieldIdent(null, "sex"), Literal(null))
  val name = FieldIdent(null, "name")
  val sex = FieldIdent(null, "sex")
  val age = FieldIdent(null, "age")
//  val star = StarExpr()

  "check everything must be ok" should {
    printTable(user)
    assert(true)
  }

  " 1 = 1 always be true" in {
    Engine.evalWhereEachRow(row1, alwaysTrue) should beTrue
  }


  " name = 'tsc' must return true for row1" in {
    Engine.evalWhereEachRow(row1, nameEqTsc) should beTrue
  }

  " name = 'tsc' return false for row2" in {
    Engine.evalWhereEachRow(row2, nameEqTsc) should beFalse
  }

  " name = 'tsc' result no empty result set" in {
    Engine.evalWhere(user, nameEqTsc).size must be_>(0)
  }

  " job = 'driver' should  return false because the table don't have" +
    "this column" in {
    Engine.evalWhereEachRow(row1, undefinedColumn) should beFalse
  }


  " sex = 'null' return true the value is null" in {
    Engine.evalWhereEachRow(row4, sexEqNull) should beTrue
  }

  " name != 'tsc' should return nonempty result set" in {
    printTable(evalWhere(user, nameNeqTsc))
    evalWhere(user, nameNeqTsc).size must be_>(0)
  }

  " sex != null should false  in row don't have sex" in {
    evalWhereEachRow(row4, sexNeqNull) should beFalse
  }

  " sex !=null should be true for row have sex and sex is not null" in {
    evalWhereEachRow(row1, sexNeqNull) should beTrue
  }

  " sex !=null should return non empty result set" in {
    evalWhere(user, sexNeqNull).size must beGreaterThanOrEqualTo(3)
  }

  " name = 'tsc' or syy = 'syy' should return at least 2 rows" in {
    val nameEqTscOrSyy = Or(Eq(name, Literal("tsc")), Eq(name, Literal("syy")))
    printTable(evalWhere(user, nameEqTscOrSyy))
    evalWhere(user, nameEqTscOrSyy).size must beGreaterThanOrEqualTo(2)
  }

  " name = 'tsc' and age=30 should return at least 1 row" in {
    val nameEqTscAndAgeEq30 = And(Eq(name, Literal("tsc")), Eq(age, Literal(30)))
    printTable(evalWhere(user, nameEqTscAndAgeEq30))
    evalWhere(user, nameEqTscAndAgeEq30).size mustEqual 1
  }

  " age < 30 should return non empty set" in {
    val ageLs30 = Ls(age,Literal(30))
    printTable(evalWhere(user,ageLs30))
    evalWhere(user, ageLs30).size must be_>(1)
  }

  " age> 29 should return one set " in {
    val ageGt29 = Gt(age,Literal(29))
    evalWhere(user,ageGt29).size mustEqual 1
  }

  " age >=30 should return one set" in {
    val ageGtEq30 = GtEq(age,Literal(30))
    evalWhere(user,ageGtEq30).size mustEqual 1
  }

  " name <='tsc' should return non empty set" in {
    val nameLsEqTsc = LsEq(name,Literal("tsc"))
    printTable(evalWhere(user,nameLsEqTsc))
    evalWhere(user,nameLsEqTsc).size must be_>(1)
  }


  "first try" in {
    val str = """ where user.name = 'tsc' and (user.age = 19 or age = 30) """
    val result = whereExpr(new lexical.Scanner(str))
    val sql = result.get
    printTable(evalWhere(user,sql))
    evalWhere(user,sql).size  must be_>(1)
  }


  " user.name = user.name " in {
    val str = """ where user.name = user.name """
    val result = whereExpr(new lexical.Scanner(str))
    val sql = result.get
    printTable(evalWhere(user,sql))
    evalWhere(user,sql).size  must beEqualTo(user.size)
  }

  "eval select" >> {
    "select * from user" in {
      val sql = Seq(StarProj())
      //printTable(evalSelect(user, sql))
      evalSelect(user, sql).size must be_>(0)
    }


    "select name as n from user "in {
      val str = """ select name as n, age as a,sex as m, "20" as twenty, "man" as mx from user"""
     // val result = projectionStatements(new lexical.Scanner(str))
     // val sql = result.get
      printTable(query(user,str))
      query(user,str).size must be_>(0)
    }

    "select name from user where sex = 'male' " in {
      val str = """ select *, "图灵" as greatman from user where sex = 'male'  and (age >2 or name = 'tsc' ) """
      printTable(query(user,str))
      query(user,str).size must be_>(0)
    }


  }



 /* "select age from user" in {
    printTable(evalSelect(user,age))
    true must beTrue
  }*/



}
