import com.jpmc.json.Json
import com.jpmc.json.Json._
import com.jpmc.parsecs.Result.Success
import org.scalatest.freespec.AnyFreeSpec

class JsonParserTest extends AnyFreeSpec {
  "Json Parsing" - {
    "Json quoted string" in {
      val str      = "\"hello world\""
      val parsed   = JsonParser.jStringStr.parse(str)
      val expected = Success(Json.String("hello world"), "")
      assert(parsed == expected)
    }

    "Json array" in {
      val str      = "[\"hi\", 123, true]"
      val parsed   = JsonParser.jArray.parse(str)
      val expected = Success(Json.Array(List(Json.String("hi"), Json.Number(123), Json.Boolean(true))), "")
      assert(parsed == expected)
    }

    "key value obj" in {
      val str      = "{\"key\": \"value\", \"key2\": \"value2\"}"
      val res      = JsonParser.jObject.parse(str)
      val expected = Success(Json.Object(Map("key" -> Json.String("value"), "key2" -> Json.String("value2"))), "")
      assert(res == expected)
    }

    "key value obj with nested object" in {
      val str = "{\"key\": [\"value1\", 123, true, null], \"key2\": {\"name\": \"Json\"}}"
      val res = JsonParser.jObject.parse(str)
      val expected = Success(
        Json.Object(
          Map(
            "key"  -> Json.Array(List(Json.String("value1"), Json.Number(123), Json.Boolean(true), Json.Null)),
            "key2" -> Json.Object(Map("name" -> Json.String("Json")))
          )
        ),
        ""
      )
      assert(res == expected)
    }
  }
  "Parsing from pretty printed Json obj" - {
    "Object works" in {
      val jsonObject = Object(
        Map(
          "name"  -> String("Json"),
          "age"   -> Number(123),
          "items" -> Array(List(String("object"), Null, Boolean(true))),
          "addressInfo" -> Object(
            Map(
              "streetName" -> String("Sun Street"),
              "country"    -> String("UK"),
              "county"     -> Null
            )
          )
        )
      )
      val printed = prettyPrint(jsonObject)
      val expected =
        """{"name": "Json", "age": 123, "items": ["object", null, true], "addressInfo": {"streetName": "Sun Street", "country": "UK", "county": null}}"""
      val parsed         = JsonParser.jObject.parse(expected)
      val expectedParsed = Success(jsonObject, "")
      assert(printed == expected)
      assert(parsed == expectedParsed)
    }
  }

}
