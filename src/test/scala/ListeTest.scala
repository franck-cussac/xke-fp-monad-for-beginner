import org.scalatest._

class ListeTest extends FunSuite with Matchers with GivenWhenThen {
  test("Un paramètre de type générique") {
    Liste(1, 2, 3)
    Liste("1", "2")
    Liste(1.0, 2.0)
    Liste()
  }

  test("Une fonction A => M[A]") {
    val l = Liste(1, 2, 3)
    val empty = Liste()

    l.head shouldEqual 1
    l.tail.head shouldEqual 2
    l.tail.tail.head shouldEqual 3
    empty shouldEqual Empty
    an [NoSuchElementException] shouldBe thrownBy(empty.head)
    an [NoSuchElementException] shouldBe thrownBy(empty.tail)
  }

  test("une fonction flatMap") {
    val l = Liste(1, 2, 3)
    val expected = Liste(1, 2, 3, 2, 3, 4, 3, 4, 5)
    val actual = l.flatMap(x => Liste(x, x+1, x+2))

    actual shouldEqual expected
  }

  test("une fonction flatMap sur Empty") {
    val empty = Liste()
    val expected = Empty
    val actual = empty.flatMap(x => Liste(x, s"$x + 1", s"$x + 2"))
  }
}
