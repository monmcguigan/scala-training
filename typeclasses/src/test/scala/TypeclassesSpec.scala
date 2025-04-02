import org.scalatest.freespec.AnyFreeSpec
class TypeclassesSpec extends AnyFreeSpec {
  "FlatMap instance for Option" - {
    val fm   = FlatMap[Option]
    val func = Option((i: Int) => i + 1)
    val ap   = fm.ap(func)
    "Some" in {
      val res = ap(Some(2))
      assert(res == Some(3))
    }
    "None" in {
      val res = ap(None)
      assert(res == None)
    }
  }
}
