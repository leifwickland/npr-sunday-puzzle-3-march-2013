
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import Main._

class MainTest extends FunSuite with ShouldMatchers {
  test("2") {
    val t = Table(2)
    t.seats should have length(2)
    t.uniqueArrangements.size should be(2)
  }

  test("3") {
    val t = Table(3)
    val a = t.uniqueArrangements
    a.size should be(6)
  }

  test("4") {
    Table(4).uniqueArrangements.size should be (9)
  }

  test("8") {
    Table(8).uniqueArrangements.size should be (49)
  }

  test("15") {
    Table(15).uniqueArrangements.size should be (1366)
  }
}
