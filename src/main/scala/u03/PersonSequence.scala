package u03

import u02.Modules.*
import Person.*
import u03.Sequences.*
import Sequence.*

object PersonSequence:

  /*
   * Get the courses of the teachers in the sequence.
   * E.g., [("Luca", 2023), ("Annalisa", "Database"), ("Davide", "Machine Learning")] => ["Database", "Machine Learning"]
   * E.g., [] => []
   */
  def getTeachersCourses(s: Sequence[Person]): Sequence[String] =
    flatMap(s)(_ match { case Teacher(_, c) =>  Cons(c, Nil()); case _ => Nil() })

  /*
   * "Fold over" the sequence, starting from the default value, by accumulating elements via binary operator.
   * E.g., [3, 7, 1, 5], 0, (_ - _) => -16
   * E.g., [], 0, (_ + _) => 0
   */
  def foldLeft[A, B](s: Sequence[A])(default: B)(op: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(op(default, h))(op)
    case _ => default

  /*
   * Get the total number of courses taught by all teachers in the sequence.
   * E.g., [("Luca", 2023), ("Annalisa", "Database"), ("Davide", "Machine Learning"), ("Elena", 2021)] => 2
   * E.g., [] => 0
   */
  def countAllTeachersCourses(s: Sequence[Person]): Int =
    foldLeft(map(filter(s)(_ match { case Teacher(_, _) => true; case _ => false }))(_ => 1))(0)(_ + _)
