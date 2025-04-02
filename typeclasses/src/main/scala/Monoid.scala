trait Monoid[A] extends Semigroup[A]:
  def neutral: A

trait Semigroup[A]:
  def combine(lhs: A, rhs: A): A
