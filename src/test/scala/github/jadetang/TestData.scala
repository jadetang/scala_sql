package github.jadetang

import MetaData._

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


  val r1:Row = Map("username"->"user1", "game"->"lol", "server"->"s1", "event"->"login")
  val r2:Row = Map("username"->"user2", "game"->"dota2", "server"->"s2", "event"->"register")
  val r3:Row = Map("username"->"user3", "game"->"lol", "server"->"s2", "event"->"login")
  val r4:Row = Map("username"->"user4", "game"->"dota2", "server"->"s3", "event"->"register")
  val r5:Row = Map("username"->"user5", "game"->"lol", "server"->"s10", "event"->"login")
  val r6:Row = Map("username"->"user6", "game"->"dota2", "server"->"s1", "event"->"login")
  val r7:Row = Map("username"->"user7", "game"->"lol", "server"->"s1", "event"->"login")
  val r8:Row = Map("username"->"user8", "game"->"lol", "server"->"s1", "event"->"login")
  val r9:Row = Map("username"->"user9", "game"->"lol", "server"->"s1", "event"->"login")
  val game: Table = List(r1, r2, r3, r4, r5,r6,r7,r8,r9)



  def printTable(table:Table) {
    println("----------------")
    println(table map ( row=> (row map(colum=>colum._1+":"+colum._2)).mkString("[",",","]")) mkString("\n"))
    println("----------------")

  }

  def main(args: Array[String]) {
     val table = Seq(user,game)
     printTable(table.reduce(_:::_))
  }



}

