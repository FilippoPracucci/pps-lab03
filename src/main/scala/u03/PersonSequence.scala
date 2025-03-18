package u03

import u02.Modules.*
import Person.*
import u03.Sequences.*
import Sequence.*

object PersonSequence:

  extension (s: Sequence[Person])

    /*
     * Get the courses of the teachers in the sequence.
     * E.g., [("Luca", 2023), ("Annalisa", "Database"), ("Davide", "Machine Learning")] => ["Database", "Machine Learning"]
     * E.g., [] => []
     */
    def getTeachersCourses: Sequence[String] =
      flatMap(s)(_ match { case Teacher(_, c) =>  Cons(c, Nil()); case _ => Nil() })

    /*
   * Get the total number of courses taught by all teachers in the sequence.
   * E.g., [("Luca", 2023), ("Annalisa", "Database"), ("Davide", "Machine Learning"), ("Elena", 2021)] => 2
   * E.g., [] => 0
   */
    def countAllTeachersCourses: Int =
      foldLeft(map(filter(s)(_ match { case Teacher(_, _) => true; case _ => false }))(_ => 1))(0)(_ + _)

  extension [A](s: Sequence[A])

    /*
     * "Fold over" the sequence, starting from the default value, by accumulating elements via binary operator.
     * E.g., [3, 7, 1, 5], 0, (_ - _) => -16
     * E.g., [], 0, (_ + _) => 0
     */
    def foldLeft[B](default: B)(op: (B, A) => B): B = s match
      case Cons(h, t) => t.foldLeft(op(default, h))(op)
      case _ => default

  // Task 3

  import Streams.*
  import Stream.*

//  def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
//    case cons(h, t) if pred(h()) => cons(h, takeWhile(t())(pred))
//    case _ => empty()

  def fill[A](n: Int)(k: A): Stream[A] = n match
    case n if n > 0 => cons(k, fill(n - 1)(k))
    case _ => empty()

  def fibonacci(): Stream[Int] =
    def _fib(n1: Int, n2: Int): Stream[Int] =
      cons(n1, _fib(n2, n1 + n2))
    _fib(0, 1)
