package github.jadetang

import AST.{FieldIdent, SqlGroupBy}
import Engine._
import TestData._
import org.junit.{BeforeClass, Assert, Before, Test}

/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
class EngineJunitTest {
  val name = FieldIdent(null, "name")
  val sex = FieldIdent(null, "sex")
  val age = FieldIdent(null, "age")

  @Before
  def printLog = {
    printTable(user)
  }

  def test(sql: String, assert: Unit) = {
    println(sql)
    printTable(query(user, sql))
    assert
  }

  @Test
  def evalGroupWithWhere = {
    val sql = "select * from user group by user.name, user.sex where sex = 'male' "
    test(sql,Assert.assertEquals(3, query(user, sql).size))
  }


  @Test
  def testMaxWithGroupBy = {
    val sql = """ select max(age) as mmmm,min(age) as yyyy,* from user group by sex,name """
    test(sql,Assert.assertTrue(query(user,sql).size ==5 ))
  }

  @Test
  def testCountStart = {
    val sql = """ select count(*) as number,name from user group by name"""
    test(sql,Assert.assertTrue(query(user,sql).size ==4 ))

  }

  @Test
  def testMax5 = {
    val sql = """ select max(5) as m,min('a') from user"""
    test(sql,Assert.assertTrue(query(user,sql).size ==1 ))
  }

  @Test
  def countUser = {
    val sql = """select count(user) as number,name from user group by name """
    test(sql,Assert.assertTrue(query(user,sql).size ==4 ))
  }

  @Test
  def countDistUser = {
    val sql = """select count(distinct name)  from user"""
    test(sql,Assert.assertTrue(query(user,sql).size ==1 ))
  }

  @Test
  def sum = {
    val sql = """ select sum(distinct age),sum(age) from user """
    test(sql,Assert.assertTrue(query(user,sql).size ==1 ))
  }

  @Test
  def avg = {
    val sql = """select avg(age),avg(distinct age),avg(5), avg(distinct 5) from user"""
    test(sql,Assert.assertTrue(query(user,sql).size ==1 ))
  }

  @Test
  def ls = {
    val sql = """ select max(age),* from user group by name  where  age<99 """
    test(sql,
      Assert.assertEquals(4, query(user, sql).size))
  }

  @Test
  def lsAndGt = {
    val sql = """ select * from user where age<30 and age>2"""
    test(sql, Assert.assertEquals(2, query(user, sql).size))
  }

  @Test
  def selectStart = {
    val sql = """  select *,max(age) from user where age<99 """
    test(sql, Assert.assertEquals(1, query(user, sql).size));
  }

  @Test
  def selectCount = {
    val sql = """select count(*) from user where age<100"""
    test(sql, Assert.assertEquals(1, query(user, sql).size));
  }

  @Test
  def groupBy = {
    val sql = """select * from user group by name """
    test(sql,"")
  }

  @Test
  def orderBy = {
    val sql = """ select * from user order by name, age  """
    test(sql,Assert.assertTrue(1==query(user, sql).head("age").value))
  }

  @Test
  def limit = {
    val sql = """ select * from user order by age limit 5"""
    test(sql,Assert.assertEquals(5,query(user,sql).size))
    val sql2 = """ select * from user order by age limit 2,4"""
    test(sql2,Assert.assertEquals(4,query(user,sql2).size))
  }

}
