case class Box[A](value: A)

trait Functor[F[_]]:
  def lift[A, B](f: A => B): F[A] => F[B]
  // lifting a function into the F[_] then using the apply function - object syntax
  // apply crosses the boundary between functions as values and objects?
  // https://stackoverflow.com/questions/9737352/what-is-the-apply-function-in-scala
  def map[A, B](fa: F[A])(f: A => B): F[B] = lift(f).apply(fa)

object Functor:
  def apply[F[_]](implicit f: Functor[F]): Functor[F] = f
  given Functor[List] with {
    override def lift[A, B](f: A => B): List[A] => List[B] = la => la.map(f)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
  given Functor[Option] with {
    override def lift[A, B](f: A => B): Option[A] => Option[B] = optA => map(optA)(f)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match
      case None    => None
      case Some(a) => Some(f(a))
  }

  given Functor[Box] with {

    override def lift[A, B](f: A => B): Box[A] => Box[B] = boxA => map(boxA)(f)

    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))

  }

trait FlatMap[F[_]] extends Apply[F]:
  def flatten[A](ffa: F[F[A]]): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    val ffb: F[F[B]] = map(fa)(f(_))
    flatten(ffb)
  def ap[A, B](f: F[A => B]): F[A] => F[B] = ba =>
    val func = map(f)(lift(_))
    val ffb  = map(func)(fab => fab(ba))
    flatten(ffb)

object FlatMap:
  def apply[F[_]](implicit f: FlatMap[F]): FlatMap[F] = f
  given FlatMap[Box] with {

    override def flatten[A](ffa: Box[Box[A]]): Box[A] = ffa.value

    override def lift[A, B](f: A => B): Box[A] => Box[B] = fa => map(fa)(f)
  }
  given FlatMap[Option] with {

    override def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa match
      case None      => None
      case Some(opt) => opt

    override def lift[A, B](f: A => B): Option[A] => Option[B] = optA =>
      optA.map(f)
      // map(optA)(f)
  }

trait Apply[F[_]] extends Functor[F]:

  def ap[A, B](f: F[A => B]): F[A] => F[B]
  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] = (fa, fb) => ap(lift(f.curried).apply(fa)).apply(fb)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    lift2(f)(fa, fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    // val productAB = product(fa, fb)
    // map2(productAB, fc) { case ((a, b), c) => f(a, b, c) }
    // alternatively:
    ap(map2(fa, fb)((a: A, b: B) => f(a, b, _)))(fc)

object Apply:
  def apply[F[_]](implicit a: Apply[F]): Apply[F] = a

trait Applicative[F[_]] extends Apply[F]:
  def pure[A](a: A): F[A]

object Applicative:
  def apply[F[_]](implicit a: Applicative[F]): Applicative[F] = a
