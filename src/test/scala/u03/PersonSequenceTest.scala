package u03

import org.junit.Test

class PersonSequenceTest:
  import u02.Modules.*
  import Person.*
  import u03.Sequences.*
  import Sequence.*

  val person1 = Student("Luca", 2023)
  val person2 = Teacher("Annalisa", "Basi di dati")
  val person3 = Teacher("Davide", "Machine Learning")
  val person4 = Student("Elena", 2021)
  val sequence: Sequence[Person] = Cons(person1, Cons(person2, Cons(person3, Cons(person4, Nil()))))

  @Test def testGetTeachersCourses() =
    assertEquals(Nil(), getTeachersCourses(Nil()))
    assertEquals(Cons("Basi di dati", Cons("Machine Learning", Nil())), getTeachersCourses(sequence))
