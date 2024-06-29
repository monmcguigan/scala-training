import Expr._
import LanguageAST._

sealed trait Expr extends Product with Serializable
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
  case class Let(name: String, value: Expr, body: Expr) extends Expr
  case class Var(name: String) extends Expr
}

case class Env(myMap: Map[String, Value]) {
  def bind(name: String, value: Value) = Env(myMap + (name -> value))

  def lookup(v: Var): Value = myMap.getOrElse(v.name, sys.error(s"Variable ${v.name} not defined in Env."))
}

object LanguageAST {
  def interpret(expr: Expr, env: Env): Value = {
    expr match {
      case Num(i) => Num(i)
      case Bool(b) => Bool(b)
      case Plus(lhs, rhs) => binOp(lhs, rhs, _ + _, env)
      case Sub(lhs, rhs) => binOp(lhs, rhs, _ - _, env)
      case Mult(lhs, rhs) => binOp(lhs, rhs, _ * _, env)
      case Div(lhs, rhs) => binOp(lhs, rhs, _ / _, env)
      case c: Cond => interpretCond(c, env)
      case eq: Eq => interpretEq(eq, env)
      case n: Not => interpretNot(n, env)
      case l: Let => interpretLet(l, env)
      case v: Var => env.lookup(v)
    }
  }

  def binOp(lhs: Expr, rhs: Expr, f: (Int, Int) => Int, env: Env): Num = {
    (interpret(lhs, env), interpret(rhs, env)) match {
      case (Num(i), Num(j)) => Num(f(i, j))
      case _ => sys.error("type error, expecting Num on both sides")
    }
  }

  def interpretLet(let: Expr.Let, env: Env): Value =
    interpret(let.body, env.bind(let.name, interpret(let.value, env)))

  def interpretCond(c: Cond, env: Env): Value = {
    interpret(c.pred, env) match {
      case Bool(b) => interpret(if (b) c.thenBranch else c.elseBranch, env)
      case _ => sys.error("type error, predicate for cond expected to be a boolean")
    }
  }

  def interpretEq(eq: Eq, env: Env): Bool = {
    (interpret(eq.lhs, env), interpret(eq.rhs, env)) match {
      case (Num(n), Num(m)) => Bool(n == m) // Numeric Equality
      case (Bool(i), Bool(j)) => Bool(i == j) // Boolean Equality
      case _ => sys.error("type error, sides of Eq aren't of same type")
    }
  }

  def interpretNot(n: Not, env: Env): Bool = {
    interpret(n.expr, env) match {
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
      case Let(name, value, body) => s"(let ${name} = ${pprint(value)}\n${pprint(body)})\n"
      case Var(name) => name
    }
  }

  def pprintVal(value: Value): String = {
    value match {
      case Num(i) => i.toString
      case Bool(b) => b.toString
    }
  }
}
object Main extends App {


  //  val exp = Plus(Sub(Num(1), Num(5)), Mult(Num(6), Sub(Num(5), Num(8))))
  //  val exp = Cond(Bool(false), Num(5), Num(6))
  //  val exp = Eq(Plus(Num(2), Num(4)), Num(6))
  val exp = Cond(Eq(Num(5), Num(7)), Plus(Num(-8), Num(14)), Mult(Num(4), Num(6)))

  /*
  * x = 5
  * x + {
  *   let y = 7
  *     y + 9
  * }
  * */
  val l = Let("x", Num(5), Plus(Var("x"), Let("y", Num(7), Plus(Var("y"), Num(9)))))

  val env = Env(Map.empty[String, Value])
  println(pprint(l))
  println(" = ")
  println(pprintVal(interpret(l, env)))
}
