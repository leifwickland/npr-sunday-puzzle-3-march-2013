import scala.collection.IndexedSeq

/**
 * @link http://puzzles.blainesville.com/2013/03/npr-sunday-puzzle-mar-3-2013-dinner.html
 * @link http://oeis.org/A048162
 */
object Main {
  def main(args: Array[String]) {
    args.toSeq match { 
      case Seq(size) => {
        val a = Table(size).uniqueArrangements
        println("A table size %s has %d arrangements.".format(size, a.size))
        a.map(_.sitters).map("  " + _).foreach(println)
      }
      case _ => println("USAGE: <table size>"); sys.exit(0)
    }
  }

  type Seats = IndexedSeq[Option[String]]
  type Stander = (Int, String)

  trait Shift {
    def seat(stander: Stander, seats: Seats): Option[Seats] = {
      shift(stander._1, seats.size) match {
        case i if (seats(i).isEmpty) => Some(seats.updated(i, Some(stander._2)))
        case _ => None
      }
    }
    protected def shift(place: Int, tableSize: Int): Int
  }

  object Left extends Shift {
    def shift(place: Int, tableSize: Int) = (place - 1) match { 
      case i if (i < 0) => (tableSize - 1) 
      case i => i 
    }
  }
  object Same extends Shift {
    def shift(place: Int, tableSize: Int) = place
  }
  object Right extends Shift {
    def shift(place: Int, tableSize: Int) = (place + 1) % tableSize
  }
  object Shifts {
    final val All = List(Left, Same, Right)
  }

  case class Table(standers: List[Stander], seats: Seats) {
    def uniqueArrangements: Set[Table] = Set(arrangements:_*)
    def arrangements: Seq[Table] = {
      standers match { 
        case Nil => Seq(this)
        case stander :: newStanders => Shifts.All.flatMap(_.seat(stander, seats)).flatMap(newSeats => Table(newStanders, newSeats).arrangements)
      }
    }
    def sitters = seats.map(_ match { case Some(name) => name case None => "" }).mkString(",")
  }

  object Table {
    def apply(standingPeople: List[Stander]): Table = new Table(standingPeople, IndexedSeq.fill(standingPeople.length)(None))
    def apply(size: Int): Table = Table(0.until(size).map(i => (i, (Character.toString(('a' + i).toChar)))).toList)
    def apply(size: String): Table = apply(size.toInt)
  }
}


