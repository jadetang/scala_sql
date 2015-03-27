package duowan

import duowan.AST.{Min, Max, SqlGroupBy, FieldIdent}
import duowan.Engine._
import duowan.TestData._
import org.junit.{Before, BeforeClass, Assert, Test}

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
    Assert.assertEquals(3,query(user,sql).size)
  }

  @Test
  def maxAndMinFunction = {
    val maxAge = Max(age)
    Assert.assertEquals(99,evalOneAggregateFuntion(user,maxAge).get("age"))
    val minAge = Min(age)
    Assert.assertEquals(1,evalOneAggregateFuntion(user,minAge).get("age"))

  }

  //@Test

}
