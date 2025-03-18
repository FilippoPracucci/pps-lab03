package u03

import PersonSequence.*
import org.junit.*
import org.junit.Assert.*

class PersonSequenceTest:
  import u02.Modules.*
  import Person.*
  import u03.Sequences.*
  import Sequence.*

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

  // Task 3

  import Streams.*
  import Streams.Stream.*

//  @Test def testTakeWhile() =
//    val stream = Stream.iterate(0)(_ + 1)
//    val expectedResult = cons(0, cons(1, cons(2, cons(3, cons(4, empty())))))
//    assertEquals(expectedResult, toList(PersonSequence.takeWhile(stream)(_ < 5)))

  @Test def testFill() =
    assertEquals(Cons(5, Cons(5, Cons(5, Nil()))), toList(fill(3)(5)))

  @Test def testFibonacci() =
    val fibonacci: Stream[Int] = PersonSequence.fibonacci()
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), toList(take(fibonacci)(5)))