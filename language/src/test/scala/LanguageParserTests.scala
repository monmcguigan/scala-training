import Expr._
import LanguageAST._
import com.jpmc.parsecs.Result._
import org.scalatest.freespec.AnyFreeSpec

class LanguageParserTests extends AnyFreeSpec {
  "Parse values" - {
    "numbers" in {
//      val nums     = Num(123)
//      val str      = pprint(nums)
//      val parsed   = LanguageParser.num.parse(str)
//      val expected = Success(nums, "")
//      assert(expected == parsed)
    }
    "bools" in {
//      val bool     = Bool(true)
//      val str      = pprint(bool)
//      val parsed   = LanguageParser.boolean.parse(str)
//      val expected = Success(bool, "")
//      assert(expected == parsed)
    }
  }
  "Parse expr - binOps" - {
    "Plus" - {
      "Simple Plus" in {
//        val plus     = Plus(Num(10), Num(5))
//        val str      = pprint(plus)
//        val parsed   = LanguageParser.binOp.parse(str)
//        val expected = Success(plus, "")
//        assert(expected == parsed)
      }
      "Nested Plus" in {
//        val plus     = Plus(Num(10), Plus(Num(5), Num(6)))
//        val str      = pprint(plus)
//        val parsed   = LanguageParser.binOp.parse(str)
//        val expected = Success(plus, "")
//        assert(expected == parsed)
      }
    }
  }
}
