import scala.annotation.tailrec

object Lab03:

  // Task 1, svolto da solo

  object Sequences:

    import u03.Optionals.*
    import Optional.*

    enum Sequence[E]:
      case Cons(head: E, tail: Sequence[E])
      case Nil()

    object Sequence:

      extension (s: Sequence[Int])
        def sum: Int = s match
          case Cons(h, t) => h + t.sum
          case _ => 0

        /*
         * Get the minimum element in the sequence
         * E.g., [30, 20, 10] => 10
         * E.g., [10, 1, 30] => 1
         */
        def min: Optional[Int] =
          @tailrec
          def _min(s: Sequence[Int])(min: Optional[Int]): Optional[Int] = s match
            case Cons(h, t) if isEmpty(min) || orElse(Optional.map(min)(_ >= h), false) => _min(t)(Just(h))
            case Cons(_, t) => _min(t)(min)
            case _ => min
          _min(s)(Empty())

      extension [A](s: Sequence[A])

        def map[B](mapper: A => B): Sequence[B] = s match
          case Cons(h, t) => Cons(mapper(h), t.map(mapper))
          case Nil() => Nil()

        def filter(pred: A => Boolean): Sequence[A] = s match
          case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
          case Cons(_, t) => t.filter(pred)
          case Nil() => Nil()

        /*
         * Skip the first n elements of the sequence
         * E.g., [10, 20, 30], 2 => [30]
         * E.g., [10, 20, 30], 3 => []
         * E.g., [10, 20, 30], 0 => [10, 20, 30]
         * E.g., [], 2 => []
         */
        def skip(n: Int): Sequence[A] = s match
          case Cons(_, t) if n > 0 => t.skip(n - 1)
          case _ => s

        /*
         * Zip two sequences
         * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
         * E.g., [10], [] => []
         * E.g., [], [] => []
         */
        def zip[B](second: Sequence[B]): Sequence[(A, B)] = (s, second) match
          case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
          case _ => Nil()

        /*
         * Concatenate two sequences
         * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
         * E.g., [10], [] => [10]
         * E.g., [], [] => []
         */
        def concat(second: Sequence[A]): Sequence[A] = (s, second) match
          case (Cons(h, t), _) => Cons(h, t.concat(second))
          case _ => second

        /*
         * Reverse the sequence
         * E.g., [10, 20, 30] => [30, 20, 10]
         * E.g., [10] => [10]
         * E.g., [] => []
         */
        def reverse: Sequence[A] =
          @annotation.tailrec
          def _rev[A](s: Sequence[A], r: Sequence[A]): Sequence[A] = s match
            case Cons(h, t) => _rev(t, Cons(h, r))
            case _ => r
          _rev(s, Nil())

        /*
         * Map the elements of the sequence to a new sequence and flatten the result
         * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
         * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
         * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
         */
        def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = s match
          case Cons(h, t) =>  mapper(h).concat(t.flatMap(mapper))
          case _ => Nil()

        /*
         * Get the elements at even indices
         * E.g., [10, 20, 30] => [10, 30]
         * E.g., [10, 20, 30, 40] => [10, 30]
         */
        def evenIndices: Sequence[A] =
          def _evenInd[A](s: Sequence[A], index: Int): Sequence[A] = s match
            case Cons(h, t) if index % 2 == 0 => Cons(h, _evenInd(t, index + 1))
            case Cons(_, t) => _evenInd(t, index + 1)
            case _ => Nil()
          _evenInd(s, 0)

        /*
         * Check if the sequence contains the element
         * E.g., [10, 20, 30] => true if elem is 20
         * E.g., [10, 20, 30] => false if elem is 40
         */
        def contains(elem: A): Boolean = s match
          case Cons(h, _) if h == elem => true
          case Cons(_, t) => t.contains(elem)
          case _ => false

        /*
         * Remove duplicates from the sequence
         * E.g., [10, 20, 10, 30] => [10, 20, 30]
         * E.g., [10, 20, 30] => [10, 20, 30]
         */
        def distinct: Sequence[A] =
          @tailrec
          def _dist(s: Sequence[A], singulars: Sequence[A]): Sequence[A] = s match
            case Cons(h, t) if !singulars.contains(h) => _dist(t, Cons(h, singulars))
            case Cons(_, t) => _dist(t, singulars)
            case _ => singulars.reverse
          _dist(s, Nil())

        /*
         * Group contiguous elements in the sequence
         * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
         * E.g., [10, 20, 30] => [[10], [20], [30]]
         * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
         */
        def group: Sequence[Sequence[A]] =
          def _group(s: Sequence[A], actual: Sequence[A]): Sequence[Sequence[A]] = (s, actual) match
            case (Cons(h, t), Cons(_, _)) if !actual.contains(h) => Cons(actual, _group(t, Cons(h, Nil())))
            case (Cons(h, t), _) => _group(t, Cons(h, actual))
            case (Nil(), Nil()) => Nil()
            case _ => Cons(actual, Nil())
          _group(s, Nil())

        /*
         * Partition the sequence into two sequences based on the predicate
         * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
         * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
         */
        def partition(pred: A => Boolean): (Sequence[A], Sequence[A]) =
          @tailrec
          def _part(s: Sequence[A], pred: A => Boolean)(part1: Sequence[A], part2: Sequence[A]): (Sequence[A], Sequence[A]) = s match
            case Cons(h, t) if pred(h) => _part(t, pred)(Cons(h, part1), part2)
            case Cons(h, t) => _part(t, pred)(part1, Cons(h, part2))
            case _ => (part1.reverse, part2.reverse)
          _part(s, pred)(Nil(), Nil())

    end Sequence
  end Sequences

  // Task 2, svolto da solo

  object PersonSequence:

    import Sequences.*
    import Sequence.*
    import u02.Modules.Person
    import Person.*

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
        s.getTeachersCourses.map(_ => 1).foldLeft(0)(_ + _)

    extension [A](s: Sequence[A])

      /*
       * "Fold over" the sequence, starting from the default value, by accumulating elements via binary operator.
       * E.g., [3, 7, 1, 5], 0, (_ - _) => -16
       * E.g., [], 0, (_ + _) => 0
       */
      def foldLeft[B](default: B)(op: (B, A) => B): B = s match
        case Cons(h, t) => t.foldLeft(op(default, h))(op)
        case _ => default

  end PersonSequence

  // Task 3, svolto da solo

  object MyStreamsExtension:

    import Sequences.*
    import Sequence.*
    import u03.Streams.*
    import Stream.*

    def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
      case Stream.Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => empty()

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case n if n > 0 => cons(k, fill(n - 1)(k))
      case _ => empty()

    def fibonacci(): Stream[Int] =
      def _fib(n1: Int, n2: Int): Stream[Int] =
        cons(n1, _fib(n2, n1 + n2))
      _fib(0, 1)

    def cycle[A](s: Sequence[A]): Stream[A] =
      def _cycle[A](toCycle: Sequence[A], original: Sequence[A]): Stream[A] = (toCycle, original) match
        case (_, Nil()) => empty()
        case (Sequence.Cons(h, t), _) => cons(h, _cycle(t, original))
        case _ => _cycle(original, original)
      _cycle(s, s)

  end MyStreamsExtension
end Lab03
