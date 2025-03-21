object Lab03:

  object Sequences: // Essentially, generic linkedlists

    import u03.Optionals.*
    import Optional.*

    enum Sequence[E]:
      case Cons(head: E, tail: Sequence[E])
      case Nil()

    object Sequence:

      extension (l: Sequence[Int])
        def sum: Int = l match
          case Cons(h, t) => h + t.sum
          case _ => 0

      extension [A](s: Sequence[A])

        def map[B](mapper: A => B): Sequence[B] = s match
          case Cons(h, t) => Cons(mapper(h), t.map(mapper))
          case Nil() => Nil()

        def filter(pred: A => Boolean): Sequence[A] = s match
          case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
          case Cons(_, t) => t.filter(pred)
          case Nil() => Nil()

        // Task 1, svolto da solo

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

    import u03.extensionmethods.Streams.*
    import Stream.*

      def takeWhile[A](s: Stream[A])(pred: A => Boolean): Stream[A] = s match
        case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
        case _ => empty()

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case n if n > 0 => cons(k, fill(n - 1)(k))
      case _ => empty()

    def fibonacci(): Stream[Int] =
      def _fib(n1: Int, n2: Int): Stream[Int] =
        cons(n1, _fib(n2, n1 + n2))
      _fib(0, 1)

  end MyStreamsExtension
end Lab03
