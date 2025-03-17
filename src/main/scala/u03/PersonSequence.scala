package u03

import u02.Modules.*
import Person.*
import u03.Sequences.*
import Sequence.*

object PersonSequence:

  def getTeacherCourses(s: Sequence[Person]): Sequence[String] = s match
    case Cons(h, t) => map(filter[Person](s)(h match { case Teacher(_, _) => h }))

