package duowan

import duowan.AST.{FieldIdent, SqlGroupBy}
import duowan.Engine._
import duowan.TestData._
import org.junit.{Assert, Before, Test}

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

  @Test
  def evalGroupByTest = {
    val groupByExpr = SqlGroupBy(Seq(name, sex))
    // evalGroupBy(user, groupByExpr) foreach (printTable(_))
    Assert.assertEquals(5, evalGroupBy(user, groupByExpr).size)
  }

  @Test
  def evalGroupWithWhere = {
    val sql = "select * from user group by user.name, user.sex where sex = 'male' "
    printTable(query(user,sql))
    Assert.assertEquals(2, query(user, sql).size)
  }



  @Test
  def testMaxWithGroupBy = {
    val sql  = """ select max(age) as mmmm,min(age) as yyyy,* from user group by sex,name """
    printTable(query(user,sql))
  }

  @Test
  def testCountStart = {
    val sql = """ select count(*) as number,name from user group by name"""
    printTable(query(user,sql))

  }

  @Test
  def testMax5 = {
    val sql =  """ select max(5) as m,min('a') from user"""
    printTable(query(user,sql))
  }

  @Test
  def countUser = {
    val sql = """select count(user) as number,name from user group by name """
    printTable(query(user,sql))
  }

  @Test
  def countDistUser = {
    val sql = """select count(distinct name)  from user"""
    printTable(query(user,sql))
  }

  @Test
  def sum = {
    val sql = """ select sum(distinct age),sum(age) from user """
    printTable(query(user,sql))
  }

  @Test
  def avg = {
    val sql = """select avg(age),avg(distinct age),avg(5), avg(distinct 5) from user"""
    printTable(query(user,sql))
  }

  @Test
  def ls = {
    val sql = """ select max(age),* from user group by name  where  age<100 """
    printTable(query(user,sql))
    Assert.assertEquals(4,query(user,sql).size)
  }

  //@Test

}
