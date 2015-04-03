package github.jadetang

/**
 * Created by jadetang on 15-3-29.
 */
object MetaData {

  type Row = Map[String, MetaData[_]]
  type Table = List[Row]
  type Schema = Map[String, Table]
  type DataBase = Map[String, Schema]


  sealed trait MetaData[T] {
    def value: T

    override def toString = value.toString

    override def hashCode = value.hashCode()

    override def equals(other: Any): Boolean = other match {
      case that: MetaData[T] => that.value == value
      case _ => false
    }
  }

  implicit class MetaInt(val v: Int) extends MetaData[Int] {
    override def value: Int = v

  }

  implicit class MetaDouble(val v: Double) extends MetaData[Double] {
    override def value: Double = v
  }

  implicit class MetaString(val v: String) extends MetaData[String] {
    override def value: String = v
  }

  implicit val listMetaOrder = new Ordering[Seq[MetaData[_]]] {
    override def compare(x: Seq[MetaData[_]], y: Seq[MetaData[_]]): Int = {

      def compareHelper(x: MetaData[_], y: MetaData[_]): Int = (x, y) match {
        case (x: MetaInt, y: MetaInt) => x.value.compareTo(y.value)
        case (x: MetaDouble, y: MetaDouble) => x.value.compareTo(y.value)
        case (x: MetaString, y: MetaString) => x.value.compareTo(y.value)
      }

      (x, y) match {
        case (x1 :: Nil, y1 :: Nil) => {
          compareHelper(x1,y1)
        }
        case (x1::tails1,y1::tails2) => {
          val result = compareHelper(x1,y1)
          if(result == 0){
            compare(tails1,tails2)
          }else{
            result
          }
        }
      }
    }
  }

  implicit val metaNumeric = new Numeric[MetaData[_]] {
    override def plus(x: MetaData[_], y: MetaData[_]): MetaData[_] = (x, y) match {
      case (x: MetaInt, y: MetaInt) => x.value + y.value
      case (x: MetaDouble, y: MetaDouble) => x.value + y.value
    }

    override def toDouble(x: MetaData[_]): Double = x match {
      case (x: MetaInt) => x.value.toDouble
      case (x: MetaDouble) => x.value
    }

    override def toFloat(x: MetaData[_]): Float = x match {
      case (x: MetaInt) => x.value.toFloat
      case (x: MetaDouble) => x.value.toFloat
    }

    override def toInt(x: MetaData[_]): Int = x match {
      case (x: MetaInt) => x.value
      case (x: MetaDouble) => x.value.toInt
    }

    override def negate(x: MetaData[_]): MetaData[_] = ???

    override def fromInt(x: Int): MetaData[_] = x

    override def toLong(x: MetaData[_]): Long = x match {
      case (x: MetaInt) => x.value.toLong
      case (x: MetaDouble) => x.value.toLong
    }

    override def times(x: MetaData[_], y: MetaData[_]): MetaData[_] = ???

    override def minus(x: MetaData[_], y: MetaData[_]): MetaData[_] = ???

    override def compare(x: MetaData[_], y: MetaData[_]): Int = (x, y) match {
      case (x: MetaInt, y: MetaInt) => x.value.compareTo(y.value)
      case (x: MetaDouble, y: MetaDouble) => x.value.compareTo(y.value)
      case (x: MetaString, y: MetaString) => x.value.compareTo(y.value)
    }
  }


}
