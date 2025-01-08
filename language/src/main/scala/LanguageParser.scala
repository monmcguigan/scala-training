//import Expr._
//import com.jpmc.parsecs.Parser
//
//object LanguageParser {
//  def num: Parser[Num] = Parser.number.map(Num)
//  val boolean          = Parser.boolean.map(Bool)
//  val value            = num | boolean
//  val binOp =
//    (expr ~ operator ~ expr).map { case ((lhs, op), rhs) =>
//      op match {
//        case '+' => Plus(lhs, rhs)
//        case '-' => Sub(lhs, rhs)
//        case '*' => Mult(lhs, rhs)
//        case '/' => Div(lhs, rhs)
//      }
//    }.rep
//  case class Thing(v: Option[String])
//  def thing(v: Option[Thing]) =
//    v match {
//      case Some(value) =>
//        value.v match {
//          case Some(value) => value
//          case None        => "hi"
//        }
//      case None => "hi"
//    }
//
//  // so we could have
//  // 1 + 2 + 3 + 4
//  // expr
//  def operator: Parser[Char] =
//    Parser.whitespace *> Parser.pred(c => c == '+' | c == '-' | c == '/' | c == '*') <* Parser.whitespace
//
//  val let =
//    ((Parser.string("let") *> (Parser.alphaNumeric *> Parser.string("=")) ~ expr <* Parser.string("in")) ~ expr).map {
//      case ((name, value), body) => Let(name, value, body)
//    }
//  val not = (Parser.string("!") *> expr).map(Not)
//  val cond = (((Parser.string("if") *> expr) <* Parser.string("then")) ~ (expr <* Parser.string("else")) ~ expr).map {
//    case ((pred, thenBranch), elseBranch) => Cond(pred, thenBranch, elseBranch)
//  }
//  def expr: Parser[Expr] = value | let | not | cond
//}
