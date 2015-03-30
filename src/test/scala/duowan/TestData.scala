package duowan

import duowan.MetaData._

/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
object TestData {
  val row1:Row = Map("name" -> "tsc", "age" -> 30, "sex" -> "male")
  val row2:Row = Map("name" -> "syy", "age" -> 29, "sex" -> "female")
  val row3:Row = Map("name" -> "dudu", "age" -> 1, "sex" -> "male")
  val row4:Row = Map("name" -> "xiaohua", "age" -> 2, "sex" -> null)
  val row5:Row = Map("name" -> "tsc", "age" -> 19, "sex" -> "male")
  val row6:Row = Map("name" -> "tsc", "age" -> 99, "sex" -> "female")
  val row7:Row = Map("name" -> "tsc", "age" -> 30, "sex" -> "female")
  val wrongRow = Map("name" -> "doudou", "age" -> 3)
  val user: Table = List(row1, row2, row3, row4, row5,row6,row7)

  def printTable(table:Table) {
    println("----------------")
    println(table map ( row=> (row map(colum=>colum._1+":"+colum._2)).mkString("[",",","]")) mkString("\n"))
    println("----------------")

  }

  def main(args: Array[String]) {
    //println(  (row1 map(x=>"'"+x._2+"'")).mkString("(",",",")") )

    user foreach(row=>print(

      (row map(x=>"'"+x._2+"'")).mkString("(",",",")")+","


    ))

  }



}

