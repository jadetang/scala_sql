package duowan

import duowan.Parser._
import org.junit.Test

/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
class PasertJunitTest {
  @Test
  def test1: Unit ={
    val str =
      """ select user.age as age, '1',1,2.2, max(name), count(distinct name), sum(age), avg(name)
          from user group by sex where age = 2 and sex = 'male'
      """
    val result = parse(str).get
    println(result)
  }


  @Test
  def selectTest: Unit= {
    val str = """ max(name) """
    val result = projectionStatements(new lexical.Scanner(str))
    //logSelect(result)
    //log(result)
    val sql = result.get
    println(sql)
   // sql.isInstanceOf[Seq[SqlProj]] should beTrue

  }

}
