package u03

import PersonSequence.*
import org.junit.*
import org.junit.Assert.*

class PersonSequenceTest:
  import u02.Modules.*
  import Person.*
  import u03.Sequences.*
  import Sequence.*

  val person1 = Student("Luca", 2023)
  val person2 = Teacher("Annalisa", "Database")
  val person3 = Teacher("Davide", "Machine Learning")
  val person4 = Student("Elena", 2021)
  val sequence: Sequence[Person] = Cons(person1, Cons(person2, Cons(person3, Cons(person4, Nil()))))

  @Test def testGetTeachersCourses() =
    assertEquals(Nil(), getTeachersCourses(Nil()))
    assertEquals(Cons("Database", Cons("Machine Learning", Nil())), getTeachersCourses(sequence))

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil ()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(0, foldLeft(Nil())(0)((x1: Int, x2: Int) => x1 + x2))

  @Test def testCountAllTeachersCourses() =
    assertEquals(2, countAllTeachersCourses(sequence))
    assertEquals(0, countAllTeachersCourses(Nil()))
