import Expr.*
import LanguageAST.*
import Value.{Bool, Lambda, Num}

import scala.::

enum Value:
  case Num(i: Int)
  case Bool(b: Boolean)
  case Lambda(param: String, body: Expr, env: Env)

enum Expr:
  case Num(i: Int)
  case Bool(b: Boolean)
  case Plus(lhs: Expr, rhs: Expr)
  case Sub(lhs: Expr, rhs: Expr)
  case Mult(lhs: Expr, rhs: Expr)
  case Div(lhs: Expr, rhs: Expr)
  case Eq(lhs: Expr, rhs: Expr)
  case Not(expr: Expr)
  case Cond(pred: Expr, thenBranch: Expr, elseBranch: Expr)
  case Let(name: String, value: Expr, body: Expr)
  case LetRec(name: String, value: Expr, valueType: Type.Function, body: Expr)
  case Var(name: String)
  case Function(param: String, paramType: Type, body: Expr)
  case Apply(func: Expr, arg: Expr)
  case Gt(lhs: Expr, rhs: Expr)

import collection.mutable

case class Bindings(name: String, var value: Value)
case class Env(map: List[Bindings]):
  def lookup(v: Var): Value =
    map.findLast(e => e.name == v.name) match
      case Some(va) => va.value
      case None     => sys.error(s"${v.name} was not found in the env: $toStringMap")
  private def toStringMap: String =
    this.map.map(t => s"${t._1} : ${pprintVal(t._2)}").mkString
  def bind(name: String, value: Value): Env =
    Env(map :+ Bindings(name, value))

  def set(name: String, value: Value) =
//    this.copy(map = map.map{ b =>
//      if (b.name == name)
//        b.value = value
//        b
//      else
//        b
//    })
    this.copy(map.map {
      case b if b.name == name =>
        b.value = value
        b
      case other => other
    })
object Env:
  val empty = Env(List.empty)

object LanguageAST

def interpret(expr: Expr, env: Env): Value =
  expr match {
    case Expr.Num(i)                    => Value.Num(i)
    case Expr.Bool(b)                   => Value.Bool(b)
    case Expr.Function(p, tpe, body)    => Value.Lambda(p, body, env)
    case Plus(lhs, rhs)                 => binOp(lhs, rhs, _ + _, env)
    case Sub(lhs, rhs)                  => binOp(lhs, rhs, _ - _, env)
    case Mult(lhs, rhs)                 => binOp(lhs, rhs, _ * _, env)
    case Div(lhs, rhs)                  => binOp(lhs, rhs, _ / _, env)
    case c: Cond                        => interpretCond(c, env)
    case eq: Eq                         => interpretEq(eq, env)
    case Expr.Gt(lhs, rhs)              => interpretGt(lhs, rhs, env)
    case n: Not                         => interpretNot(n, env)
    case v: Var                         => env.lookup(v)
    case Apply(func, arg)               => interpretApply(func, arg, env)
    case l: Let                         => interpretLet(l, env)
    case LetRec(name, value, tpe, body) => interpretLetRec(name, value, body, env)

  }
def interpretGt(lhs: Expr, rhs: Expr, env: Env): Value = (interpret(lhs, env), interpret(rhs, env)) match
  case (Value.Num(l), Value.Num(r)) => Value.Bool(l > r)
  case _                            => sys.error(s"Invalid types to compare ${pprint(lhs)} and ${pprint(rhs)}")

def interpretLetRec(name: String, value: Expr, body: Expr, env: Env): Value =
  val newEnv = env.bind(name, null)     // bind the func name to null value
  val v      = interpret(value, newEnv) // interpret the value in the env where name is defined in the env
  newEnv.set(name, v)     // set the interpreted value to the name
  interpret(body, newEnv) // interpret the body in env with the function defined

//  val newEnv = env.bind(name, null)
//
//  interpret(value, newEnv) match
//    case function: Value.Function =>
//      newEnv.set(name, function)
//      interpret(body, newEnv)
//
//    case _ => sys.error("Type error in LetRec")
def interpretApply(func: Expr, arg: Expr, env: Env): Value =
  interpret(func, env) match
    case Value.Lambda(param, body, closedEnv) =>
      val argValue = interpret(arg, env)
      val funEnv   = closedEnv.bind(param, argValue)
      interpret(body, funEnv)
    case other => sys.error(s"$other is not a function, cannot be applied, ${pprint(func)} and ${pprint(arg)}")

def binOp(lhs: Expr, rhs: Expr, f: (Int, Int) => Int, env: Env): Num =
  (interpret(lhs, env), interpret(rhs, env)) match {
    case (Num(i), Num(j)) => Num(f(i, j))
    case _                => sys.error("type error, expecting Num on both sides")
  }

def interpretLet(let: Let, env: Env): Value =
  interpret(let.body, env.bind(let.name, interpret(let.value, env)))

def interpretCond(c: Cond, env: Env): Value =
  interpret(c.pred, env) match {
    case Bool(b) => interpret(if (b) c.thenBranch else c.elseBranch, env)
    case _       => sys.error("type error, predicate for cond expected to be a boolean")
  }

def interpretEq(eq: Eq, env: Env): Bool =
  (interpret(eq.lhs, env), interpret(eq.rhs, env)) match {
    case (Num(n), Num(m))   => Bool(n == m) // Numeric Equality
    case (Bool(i), Bool(j)) => Bool(i == j) // Boolean Equality
    case _                  => sys.error("type error, sides of Eq aren't of same type")
  }

def interpretNot(n: Not, env: Env): Bool =
  interpret(n.expr, env) match {
    case Bool(b) => Bool(!b)
    case _       => sys.error("type error, not equals expecting a boolean expr")
  }

def pprint(expr: Expr): String =
  expr match {
    case n: Expr.Num    => pprintVal(Num(n.i))
    case b: Expr.Bool   => pprintVal(Bool(b.b))
    case Plus(lhs, rhs) => s"${pprint(lhs)} + ${pprint(rhs)}"
    case Sub(lhs, rhs)  => s"${pprint(lhs)} - ${pprint(rhs)}"
    case Mult(lhs, rhs) => s"(${pprint(lhs)} * ${pprint(rhs)})"
    case Div(lhs, rhs)  => s"(${pprint(lhs)} / ${pprint(rhs)})"
    case Cond(pred, thenBranch, elseBranch) =>
      s"if ${pprint(pred)} then ${pprint(thenBranch)} else ${pprint(elseBranch)}"
    case Eq(lhs, rhs)                    => s"${pprint(lhs)} == ${pprint(rhs)}"
    case Not(expr: Expr)                 => s"!${pprint(expr)}"
    case Let(name, value, body)          => s"let $name = ${pprint(value)} in \n ${pprint(body)}"
    case Var(name)                       => name
    case Expr.Function(param, tpe, body) => s"$param -> ${pprint(body)}"
    case Apply(f, arg)                   => s"${pprint(f)} (${pprint(arg)})"
    case Gt(lhs, rhs)                    => s"${pprint(lhs)} > ${pprint(rhs)}"
    case LetRec(name, value, tpe, body)  => s"let rec $name = ${pprint(value)} in \n ${pprint(body)}"
  }

def pprintVal(value: Value): String =
  value match {
    case Num(i)                      => i.toString
    case Bool(b)                     => b.toString
    case Value.Lambda(name, body, _) => s"$name => ${pprint(body)}"
  }
