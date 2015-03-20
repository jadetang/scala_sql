package duowan

import duowan.Engine._

/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
object TestData {
  val row1 = Map("name" -> "tsc", "age" -> 30, "sex" -> "male")
  val row2 = Map("name" -> "syy", "age" -> 29, "sex" -> "female")
  val row3 = Map("name" -> "dudu", "age" -> 1, "sex" -> "male")
  val row4 = Map("name" -> "xiaohua", "age" -> 2, "sex" -> null)
  val row5 = Map("name" -> "tsc", "age" -> 19, "sex" -> "male")
  val wrongRow = Map("name" -> "doudou", "age" -> 3)
  val user: Table = List(row1, row2, row3, row4, row5)

  def printTable(table:Table) {
    println("----------------")
    println(table map ( row=> (row map(colum=>colum._1+":"+colum._2)).mkString("[",",","]")) mkString("\n"))
    println("----------------")

  }

}

