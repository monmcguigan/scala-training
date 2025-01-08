import scala.annotation.tailrec

enum Order:
  case GT
  case LT
  case EQ

case class SemanticVersion(major: Int, minor: Int, patch: Int)

trait Ordering[A]:
  def compare(a: A, b: A): Order
  def contramap[B](f: B => A): Ordering[B] = (b1, b2) => compare(f(b1), f(b2))
  def invert: Ordering[A] = (lhs, rhs) =>
    this.compare(lhs, rhs) match
      case Order.GT => Order.LT
      case Order.LT => Order.GT
      case other    => other

  extension (a: A)
    def >(b: A): Boolean =
      compare(a, b) == Order.GT
    def <(b: A): Boolean =
      compare(a, b) == Order.LT

    def eq(b: A): Boolean =
      compare(a, b) == Order.EQ
    def max(b: A): A =
      compare(a, b) match
        case Order.GT => a
        case Order.LT => b
        case Order.EQ => a

    def min(b: A): A =
      compare(a, b) match
        case Order.GT => b
        case Order.LT => a
        case Order.EQ => a
    def compareTo(b: A): Order =
      compare(a, b)

object Ordering extends App:
  def apply[A](implicit o: Ordering[A]): Ordering[A] = o

  given intOrdering: Ordering[Int] = (lhs, rhs) =>
    if lhs > rhs then Order.GT
    else if lhs < rhs then Order.LT
    else Order.EQ

  given strOrdering: Ordering[String] = (lhs, rhs) =>
    if (lhs < rhs) Order.LT
    else if (lhs > rhs) Order.GT
    else Order.EQ

  given semVerOrdering: Ordering[SemanticVersion] =
    (a, b) =>
      intOrdering.compare(a.major, b.major) match
        case Order.EQ =>
          intOrdering.compare(a.minor, b.minor) match
            case Order.EQ => intOrdering.compare(a.patch, b.patch)
            case other    => other
        case other => other

  @tailrec
  def bubbleSort[A](as: List[A])(implicit ordering: Ordering[A]): List[A] =
    def swap(myAs: List[A]): List[A] =
      myAs match
        case a :: b :: rest if ordering.compare(a, b) == Order.GT => b :: swap(a :: rest)
        case x :: xs                                              => x :: swap(xs)
        case Nil                                                  => Nil
    val sortedList = swap(as)
    if (sortedList == as) as
    else bubbleSort(sortedList)

  // String ordering
  //  println(implicitly[Ordering[String]].compare("hello", "goodbye"))
  //  println("hello".max("goodbye")) // "hello"
  //  println("hello".min("goodbye")) // "goodbye"

  // Int ordering
  println(20.max(10)) // 20
  println(20.min(10)) // 10
  val listToSort = List(2, 5, 1, 6, 2, 6)
  println(bubbleSort(listToSort)) // List(1, 2, 2, 5, 6, 6)

  // Semantic Version ordering
  val semVer1 = SemanticVersion(1, 0, 0)
  val semVer2 = SemanticVersion(1, 0, 1)
  println(semVer1.max(semVer2)) // 1.0.1
  println(semVer1.min(semVer2)) // 1.0.0
  val semVer3 = SemanticVersion(1, 1, 1)
  val semVer4 = SemanticVersion(0, 1, 1)
  println(semVer1.min(semVer2)) // 1.0.0
  println(semVer1.max(semVer2)) // 1.0.1
  val semVers = List(semVer1, semVer2, semVer3, semVer4)
  println(bubbleSort(semVers)) // 0.1.1, 1.0.0, 1.0.1, 1.1.1