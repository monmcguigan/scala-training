import Expr.Function
import Type.Num

enum Type:
  case Num
  case Bool
  case Function(from: Type, to: Type)

case class TypeEnv(env: Map[String, Type]):
  def bind(name: String, tpe: Type): TypeEnv = TypeEnv(env + (name -> tpe))
  def lookup(name: String): Either[String, Type] = env.get(name) match
    case Some(tpe) => Right(tpe)
    case None      => Left(s"$name not found in typeEnv")

object TypeEnv:
  val empty = TypeEnv(Map.empty)
object TypeChecker {
  def typeCheck(expr: Expr, env: TypeEnv): Either[String, Type] = expr match
    case Expr.Num(i)                              => Right(Type.Num)
    case Expr.Bool(b)                             => Right(Type.Bool)
    case Expr.Plus(lhs, rhs)                      => checkNumerical(lhs, rhs, env)
    case Expr.Sub(lhs, rhs)                       => checkNumerical(lhs, rhs, env)
    case Expr.Mult(lhs, rhs)                      => checkNumerical(lhs, rhs, env)
    case Expr.Div(lhs, rhs)                       => checkNumerical(lhs, rhs, env)
    case Expr.Eq(lhs, rhs)                        => checkEq(lhs, rhs, env)
    case Expr.Not(expr)                           => checkNot(expr, env)
    case Expr.Cond(pred, thenBranch, elseBranch)  => checkCond(pred, thenBranch, elseBranch, env)
    case Expr.Let(name, value, body)              => checkLet(name, value, body, env)
    case Expr.LetRec(name, value, valueTpe, body) => checkLetRec(name, value, valueTpe, body, env)
    case Expr.Var(name)                           => checkVar(name, env)
    case Expr.Function(name, paramTpe, body)      => checkLambda(name, paramTpe, body, env)
    case Expr.Apply(func, arg)                    => checkApply(func, arg, env)
    case Expr.Gt(lhs, rhs)                        => checkGt(lhs, rhs, env)

  private def expect(expr: Expr, expected: Type, typeEnv: TypeEnv): Either[String, Type] =
    typeCheck(expr, typeEnv).flatMap { observed =>
      Either.cond(test = observed == expected, left = s"expected: $expected, found: $observed", right = observed)
    }

  private def checkNumerical(lhs: Expr, rhs: Expr, typeEnv: TypeEnv): Either[String, Type] = for {
    _ <- expect(lhs, Type.Num, typeEnv)
    _ <- expect(rhs, Type.Num, typeEnv)
  } yield Type.Num

  private def checkGt(lhs: Expr, rhs: Expr, typeEnv: TypeEnv) = for {
    _ <- expect(lhs, Type.Num, typeEnv)
    _ <- expect(rhs, Type.Num, typeEnv)
  } yield Type.Bool

  private def checkEq(lhs: Expr, rhs: Expr, typeEnv: TypeEnv) = for {
    _ <- expect(lhs, Type.Num, typeEnv)
    _ <- expect(rhs, Type.Num, typeEnv)
  } yield Type.Bool

  private def checkCond(pred: Expr, thenBranch: Expr, elseBranch: Expr, typeEnv: TypeEnv) = for {
    _ <- expect(pred, Type.Bool, typeEnv)
    x <- typeCheck(thenBranch, typeEnv)
    _ <- expect(elseBranch, x, typeEnv)
  } yield x

  private def checkNot(expr: Expr, typeEnv: TypeEnv) = for {
    _ <- expect(expr, Type.Bool, typeEnv)
  } yield Type.Bool

  private def checkLet(name: String, value: Expr, body: Expr, typeEnv: TypeEnv) = for {
    x <- typeCheck(value, typeEnv)
    y <- typeCheck(body, typeEnv.bind(name, x))
  } yield y

  private def checkVar(name: String, typeEnv: TypeEnv) = typeEnv.lookup(name)

  private def checkLambda(name: String, paramType: Type, body: Expr, typeEnv: TypeEnv) =
    typeCheck(body, typeEnv.bind(name, paramType)).map { x =>
      Type.Function(paramType, x)
    }

  private def checkFunc(expr: Expr, typeEnv: TypeEnv) =
    typeCheck(expr, typeEnv).flatMap {
      case f: Type.Function => Right(f)
      case other            => Left(s"${pprint(expr)} was not a function")
    }

  private def checkApply(function: Expr, arg: Expr, typeEnv: TypeEnv) = checkFunc(function, typeEnv).flatMap {
    case Type.Function(x, y) => expect(arg, x, typeEnv).map(_ => y)
  }

  private def checkLetRec(name: String, value: Expr, valueType: Type.Function, body: Expr, typeEnv: TypeEnv) =
    val newEnv = typeEnv.bind(name, valueType)
    for {
      _ <- expect(value, valueType, typeEnv)
      y <- typeCheck(body, newEnv)
    } yield y

}
