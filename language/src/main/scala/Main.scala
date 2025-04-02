import Expr.*

object Main extends App {
  val env = Env.empty
  scala.Ordering
// let x = 1 in
//    (let x = 2 in x) + x
  val checkScope =
    Let(name = "x", value = Num(1), body = Plus(Let(name = "x", value = Num(2), body = Var("x")), Var("x")))
  println(pprintVal(interpret(checkScope, env)))
  // this gives us 3 as expected

  // let rec sum = lower -> upper -> let foo = 1 in
  //  if lower > upper then 0
  //  else                  lower + (sum (lower + 1) upper)
  //  in sum 1 10

  val checkList = LetRec(
    name = "sum",
    value = Function(
      param = "lower",
      paramType = Type.Num,
      body = Function(
        param = "upper",
        paramType = Type.Num,
        body = Let(
          name = "foo",
          value = Num(1),
          body = Cond(
            pred = Expr.Gt(lhs = Var("lower"), rhs = Var("upper")),
            thenBranch = Num(0),
            elseBranch = Plus(
              lhs = Var("lower"),
              rhs = Apply(func = Apply(func = Var("sum"), arg = Plus(Var("lower"), Num(1))), arg = Var("upper"))
            )
          )
        )
      )
    ),
    valueType = Type.Function(Type.Num, Type.Function(Type.Num, Type.Num)),
    body = Apply(Apply(Var("sum"), Num(1)), Num(10))
  )

  println(pprint(checkList))
  println("=")
  println(pprintVal(interpret(checkList, env)))
//
//  val typedLetRec = LetRec(
//    name = "sum",
//    valueType = Type.Function(Type.Num, Type.Function(Type.Num, Type.Num)),
//    value = Lambda(
//      param = "lower",
//      paramType = Type.Num,
//      body = Lambda(
//        param = "upper",
//        paramType = Type.Num,
//        body = Cond(
//          pred = Gt(Var("lower"), Var("upper")),
//          thenBranch = Num(0),
//          elseBranch = Plus(
//            lhs = Var("lower"),
//            rhs = Apply(func = Apply(Var("sum"), Plus(Var("lower"), Num(1))), arg = Var("upper"))
//          )
//        )
//      )
//    ),
//    body = Apply(Apply(Var("sum"), Num(1)), Num(10))
//  )
//  println(pprint(typedLetRec))
//  println(println(TypeChecker.typeCheck(typedLetRec, TypeEnv.empty)))
//  println("=")
//  println(pprintVal(interpret(typedLetRec, env)))

}
