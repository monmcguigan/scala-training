import Expr._

sealed trait Expr
sealed trait Value
object Expr {
  case class Num(i: Int) extends Expr with Value
  case class Bool(b: Boolean) extends Expr with Value
  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr
  case class Mult(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr
  case class Eq(lhs: Expr, rhs: Expr) extends Expr
  case class Not(expr: Expr) extends Expr
  case class Cond(pred: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr
}

object Main extends App {
  def interpret(expr: Expr): Value = {
    expr match {
      case Num(i) => Num(i)
      case Bool(b) => Bool(b)
      case Plus(lhs, rhs) => binOp(lhs, rhs, _ + _)
      case Sub(lhs, rhs) => binOp(lhs, rhs, _ - _)
      case Mult(lhs, rhs) => binOp(lhs, rhs, _ * _)
      case Div(lhs, rhs) => binOp(lhs, rhs, _ / _)
      case c: Cond => interpretCond(c)
      case eq: Eq => interpretEq(eq)
      case n: Not => interpretNot(n)
    }
  }
  def binOp(lhs: Expr, rhs: Expr, f: (Int, Int) => Int): Num = {
    (interpret(lhs), interpret(rhs)) match {
      case (Num(i), Num(j)) => Num(f(i, j))
      case _ => sys.error("type error, expecting Num on both sides")
    }
  }
  def interpretCond(c: Cond): Value = {
    interpret(c.pred) match {
      case Bool(b) => interpret(if(b) c.thenBranch else c.elseBranch)
      case _ => sys.error("type error, predicate for cond expected to be a boolean")
    }
  }
  def interpretEq(eq: Eq): Bool = {
    (interpret(eq.lhs), interpret(eq.rhs)) match {
      case (Num(n), Num(m)) => Bool(n == m) // Numeric Equality
      case (Bool(i), Bool(j)) => Bool(i == j) // Boolean Equality
      case _ => sys.error("type error, sides of Eq aren't of same type")
    }
  }
  def interpretNot(n: Not): Bool = {
    interpret(n.expr) match {
      case Bool(b) => Bool(!b)
      case _ => sys.error("type error, not equals expecting a boolean expr")
    }
  }
  def pprint(expr: Expr): String = {
    expr match {
      case n: Expr.Num => pprintVal(n)
      case b: Expr.Bool => pprintVal(b)
      case Expr.Plus(lhs, rhs) => s"${pprint(lhs)} + ${pprint(rhs)}"
      case Expr.Sub(lhs, rhs) => s"${pprint(lhs)} - ${pprint(rhs)}"
      case Expr.Mult(lhs, rhs) => s"(${pprint(lhs)} * ${pprint(rhs)})"
      case Expr.Div(lhs, rhs) => s"(${pprint(lhs)} / ${pprint(rhs)})"
      case Expr.Cond(pred, thenBranch, elseBranch) => s"if ${pprint(pred)} then ${pprint(thenBranch)} else ${pprint(elseBranch)}"
      case Expr.Eq(lhs, rhs) => s"${pprint(lhs)} == ${pprint(rhs)}"
      case Not(expr: Expr) => s"!${pprint(expr)}"
    }
  }

  def pprintVal(value: Value): String = {
    value match {
      case Num(i) => i.toString
      case Bool(b) => b.toString
    }
  }

//  val exp = Plus(Sub(Num(1), Num(5)), Mult(Num(6), Sub(Num(5), Num(8))))
//  val exp = Cond(Bool(false), Num(5), Num(6))
//  val exp = Eq(Plus(Num(2), Num(4)), Num(6))
  val exp = Cond(Eq(Num(5), Num(7)), Plus(Num(-8), Num(14)), Mult(Num(4), Num(6)))

  println(pprint(exp))
  println(" = ")
  println(pprintVal(interpret(exp)))
}
