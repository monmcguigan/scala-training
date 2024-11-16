import org.scalatest.freespec.AnyFreeSpec
import Expr.*
import Value.*
class InterpreterTest extends AnyFreeSpec {

  "let statements and scope" - {
    "simple scope, no shadowing" in {
      // let x = 1 in
      //    3 + x
      val env = Env.empty
      val letExpr =
        Let(name = "x", value = Expr.Num(1), body = Plus(Expr.Num(3), Var("x")))
      val interpreted = interpret(letExpr, env)
      val expected    = Value.Num(4)
      assert(interpreted == expected)
    }
    "shadowing" in {
      // let x = 1 in
      //    (let x = 2 in x) + x
      val env = Env.empty
      val letExpr =
        Let(
          name = "x",
          value = Expr.Num(1),
          body = Plus(Let(name = "x", value = Expr.Num(2), body = Var("x")), Var("x"))
        )
      val interpreted = interpret(letExpr, env)
      val expected    = Value.Num(3)
      assert(interpreted == expected)
    }
  }

  "lambda elimination and closures" - {
    "close over the env correctly" in {
      // let y = 1 in
      //  let f = x -> x + y in
      //    let y = 2 in
      //      f 3 == 4
      val env = Env.empty
      val applyExpr = Let(
        name = "y",
        value = Expr.Num(1),
        body = Expr.Let(
          name = "f",
          value = Expr.Function(param = "x", paramType = Type.Num, body = Plus(Expr.Var("x"), Expr.Var("y"))),
          body = Let(name = "y", value = Expr.Num(2), body = Expr.Apply(Var("f"), Expr.Num(3)))
        )
      )
      val interpreted = interpret(applyExpr, env)
      val expected    = Value.Num(4)
      assert(interpreted == expected)
    }
  }
  "recursive functions" - {
    "summing numbers in a range" in {
      // let rec sum = lower -> upper ->
      //    if lower > upper then 0
      //    else lower + (sum (lower + 1) upper)
      //  in sum 1 10
      val letRec = LetRec(
        name = "sum",
        value = Expr.Function(
          param = "lower",
          paramType = Type.Num,
          body = Expr.Function(
            param = "upper",
            paramType = Type.Num,
            body = Expr.Cond(
              pred = Expr.Gt(Expr.Var("lower"), Expr.Var("upper")),
              thenBranch = Expr.Num(0),
              elseBranch = Plus(
                lhs = Expr.Var("lower"),
                rhs = Apply(func = Apply(func = Var("sum"), arg = Plus(Var("lower"), Expr.Num(1))), arg = Var("upper"))
              )
            )
          )
        ),
        valueType = Type.Function(Type.Num, Type.Num),
        body = Apply(Apply(Var("sum"), Expr.Num(1)), Expr.Num(10))
      )
      val interpreted = interpret(letRec, Env.empty)
      val expected    = Value.Num(55)
      assert(interpreted == expected)
    }
    "find some other numberical recursion" in {}
  }
}
