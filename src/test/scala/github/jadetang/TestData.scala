package github.jadetang

import java.util.Date

import github.jadetang.MetaData._
import org.joda.time.DateTime

/**
 *
 * @author tangsicheng
 * @version 1.0    
 * @since 1.0
 */
object TestData {
  val row1: Row = Map("name" -> "tsc", "age" -> 30, "sex" -> "male", "birthDay" -> toDate(1985, 3, 4))
  val row2: Row = Map("name" -> "syy", "age" -> 29, "sex" -> "female", "birthDay" -> toDate(1986, 1, 19))
  val row3: Row = Map("name" -> "dudu", "age" -> 1, "sex" -> "male", "birthDay" -> toDate(2014, 6, 20))
  val row4: Row = Map("name" -> "xiaohua", "age" -> 2, "sex" -> null, "birthDay" -> toDate(2015, 6, 20))
  val row5: Row = Map("name" -> "tsc", "age" -> 19, "sex" -> "male", "birthDay" -> toDate(1990, 2, 18))
  val row6: Row = Map("name" -> "tsc", "age" -> 99, "sex" -> "female", "birthDay" -> toDate(1995, 3, 2))
  val row7: Row = Map("name" -> "tsc", "age" -> 30, "sex" -> "female", "birthDay" -> toDate(1989, 4, 2))
  val wrongRow = Map("name" -> "doudou", "age" -> 3)
  val user: Table = List(row1, row2, row3, row4, row5, row6, row7)

  def printTable(table: Table) {
    println("----------------")
    println(table map (row => (row map (colum => colum._1 + ":" + colum._2)).mkString("[", ",", "]")) mkString ("\n"))
    println("----------------")

  }

  def toDate(yyyy: Int, mm: Int, dd: Int): Date = {
    new DateTime(yyyy, mm, dd,0,0).toDate
  }

  def main(args: Array[String]) {
    //println(  (row1 map(x=>"'"+x._2+"'")).mkString("(",",",")") )

    user foreach (row => print(

      (row map (x => "'" + x._2 + "'")).mkString("(", ",", ")") + ","


    ))

  }


}

