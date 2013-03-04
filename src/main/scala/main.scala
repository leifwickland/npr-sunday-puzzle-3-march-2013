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

  case class Table(standers: List[Stander], seats: Seats) {
    def size = seats.size
    def sitters = seats.map(_ match { case Some(name) => name case None => "" }).mkString(",")
    def uniqueArrangements: Set[Table] = Table.arrangements(List(this)).toSet
    type Move = Int => Int
    private val moves: Seq[Move] = Seq(
      { place => (place + 1) % this.size }, // Right
      { place => place }, // Same
      { place => (place - 1) match { case i if (i < 0) => (this.size - 1) case i => i  } } // Left
    )
    def seatNext: Seq[Table] = {
      val (standerSpot, standerName) :: stillStanding = standers
      moves.
        map(_(standerSpot)).
        collect { case seatedSpot if (seats(seatedSpot).isEmpty) => Table(stillStanding, seats.updated(seatedSpot, Some(standerName))) }
    }
  }

  object Table {
    def apply(standingPeople: List[Stander]): Table = new Table(standingPeople, IndexedSeq.fill(standingPeople.length)(None))
    def apply(size: Int): Table = Table(0.until(size).map(i => (i, (Character.toString(('a' + i).toChar)))).toList)
    def apply(size: String): Table = apply(size.toInt)

    def arrangements(tables: List[Table]): Seq[Table] = arrangements(tables, List())
    @annotation.tailrec
    private def arrangements(tables: List[Table], valid: Seq[Table]): Seq[Table] = {
      tables match {
        case Nil => valid
        case table :: moreTables => {
          if (table.standers.isEmpty) arrangements(moreTables, table +: valid)
          else arrangements(table.seatNext ++: moreTables, valid)
        }
      }
    }
  }
}
