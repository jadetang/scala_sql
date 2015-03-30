package duowan

import org.junit.Test
/**
 * Created by jadetang on 15-3-29.
 */
class MetaDataTest {
  import duowan.TestData._
  import duowan.MetaData._

  @Test
  def testMaxAndMin = {
    val x = user.maxBy(row=>row("age")).collect{ case("age",_2)=>("xxx",_2)}
    val m:Row = Map(x.head._1->x.head._2)
    println(user.maxBy(row=>row("age")))
    println(user.minBy(row=>row("age")))
  }

  @Test
  def testSum = {
    println(user map(_("age")) sum)
  }

  @Test
  def orderBy = {
    printTable(user)
    val keys = List("age","name")
    printTable(user.sortBy(row=>keys.map(row(_))))
  }

}
