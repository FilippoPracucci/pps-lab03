import Lab03.Sequences.*
import Sequence.*
import org.junit.*
import org.junit.Assert.*

class SequenceTest:

  import u03.Optionals.*
  import Optional.*

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sequence.sum)

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), sequence.map(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), sequence.map(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), sequence.filter(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), sequence.filter(_ != 20))

  // Task 1, svolto da solo

  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), sequence.skip(2))
    assertEquals(Nil(), sequence.skip(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), sequence.skip(0))
    assertEquals(Nil(), Nil().skip(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), sequence.zip(l2))
    assertEquals(Nil(), sequence.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), sequence.concat(l2))
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), sequence.reverse)
    assertEquals(Nil(), Nil().reverse)

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), sequence.flatMap(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), Nil().flatMap(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), sequence.min)
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testEvenIndices() =
    assertEquals(Cons(10, Cons(30, Nil())), sequence.evenIndices)
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains() =
    assertEquals(true, sequence.contains(10))
    assertEquals(false, sequence.contains(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct() =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), sequence.distinct)
    assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

end SequenceTest

// Task 2, svolto da solo

class PersonSequenceTest:

  import u02.Modules.*
  import Person.*
  import Lab03.PersonSequence.*

  private val person1 = Student("Luca", 2023)
  private val person2 = Teacher("Annalisa", "Database")
  private val person3 = Teacher("Davide", "Machine Learning")
  private val person4 = Student("Elena", 2021)
  private val sequence = Cons(person1, Cons(person2, Cons(person3, Cons(person4, Nil()))))

  @Test def testGetTeachersCourses() =
    assertEquals(Nil(), getTeachersCourses(Nil()))
    assertEquals(Cons("Database", Cons("Machine Learning", Nil())), sequence.getTeachersCourses)

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil ()))))
    assertEquals(-16, lst.foldLeft(0)(_ - _))
    assertEquals(0, foldLeft(Nil())(0)((x1: Int, x2: Int) => x1 + x2))

  @Test def testCountAllTeachersCourses() =
    assertEquals(2, sequence.countAllTeachersCourses)
    assertEquals(0, countAllTeachersCourses(Nil()))

end PersonSequenceTest

// Task 3, svolto da solo

class MyStreamExtensionTest:

  import u03.extensionmethods.Streams.*
  import Stream.*
  import Lab03.MyStreamsExtension.*

  @Test def testTakeWhile() =
    val stream = Stream.iterate(0)(_ + 1)
    val expectedResult = cons(0, cons(1, cons(2, cons(3, cons(4, empty())))))
    assertEquals(toList(expectedResult), toList(takeWhile(stream)(_ < 5)))

  @Test def testFill() =
    val expectedResult = cons(5, cons(5, cons(5, empty())))
    assertEquals(toList(expectedResult), Stream.toList(fill(3)(5)))

  @Test def testFibonacci() =
    val expectedResult = cons(0, cons(1, cons(1, cons(2, cons(3, empty())))))
    assertEquals(toList(expectedResult), Stream.toList(Stream.take(fibonacci())(5)))

end MyStreamExtensionTest
