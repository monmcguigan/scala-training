import com.jpmc.json.Json
import com.jpmc.parsecs.Result.Success
import org.scalatest.freespec.AnyFreeSpec

class JsonParserTest extends AnyFreeSpec {
  "Json Parsing" ignore {
    "Json quoted string" in {
      val str = "\"hello world\""
      val parsed = JsonParser.jStringStr.parse(str)
      val expected = Success(Json.String("hello world"), "")
      assert(parsed == expected)
    }
  }
}