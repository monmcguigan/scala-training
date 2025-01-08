package com.jpmc.parsecs

import com.jpmc.parsecs.Parser.{string, whitespace}
import com.jpmc.parsecs.Result.Success
import org.scalatest.freespec.AnyFreeSpec

class ParserTests extends AnyFreeSpec {
  "Parse string" in {
    val str      = "Hello"
    val result   = Parser.string("Hello").parse(str)
    val expected = Result.Success("Hello", "")
    assert(result == expected)
  }
  "Parse string partial" in {
    val str      = "Hello"
    val result   = Parser.string("Hell").parse(str)
    val expected = Result.Success("Hell", "o")
    assert(result == expected)
  }
  "Parse Char" in {
    val c        = "c".toCharArray.head
    val result   = Parser.char(c).parse("c")
    val expected = Result.Success(c, "")
    assert(result == expected)
  }
  "Parse Int" in {
    val num      = "123"
    val result   = Parser.number.parse(num)
    val expected = Result.Success(123, "")
    assert(result == expected)
  }
  "Parse Boolean" - {
    "true" in {
      val t        = "true"
      val result   = Parser.boolean.parse(t)
      val expected = Result.Success(true, "")
      assert(result == expected)
    }
    "false" in {
      val f        = "false"
      val result   = Parser.boolean.parse(f)
      val expected = Result.Success(false, "")
      assert(result == expected)
    }
    "faalse" in {
      val f        = "faalse"
      val result   = Parser.boolean.parse(f)
      val expected = Result.Failure("Unable to parse input: alse")
      assert(result == expected)
    }
  }
  "Parse Name" - {
    "Success" in {
      val firstName = "Monica"
      val lastName  = "McGuigan"
      def parseName(firstName: String, secondName: String): Parser[(String, String)] =
        (string(firstName) <* whitespace.rep0) ~ string(secondName)

      val result   = parseName(firstName, lastName).parse("MonicaMcGuigan")
      val expected = Success(("Monica", "McGuigan"), "")
      assert(result == expected)
    }
  }
  "Parse opt" in {
    val str      = "string"
    val res      = Parser.string("string").opt.parse(str)
    val expected = Success(Some(str), "")
    assert(res == expected)
  }

  "List" - {
    "alphanumeric with spaces" in {
      val str      = "huu ii889widiwf9"
      val res      = Parser.alphaNumeric.parse(str)
      val expected = Success(str, "")
      assert(res == expected)
    }
    "List of strings" in {
      val str      = "s, g, h, a"
      val res      = Parser.listContent.parse(str)
      val expected = Success(List("s", "g", "h", "a"), "")
      assert(res == expected)
    }
    "Empty List [] - e.g. Json.Array(List())" in {
      val str      = ""
      val res      = Parser.listContent.parse(str)
      val expected = Success(List.empty[String], "")
      assert(res.map(_.length) == expected.map(_.length))
      assert(res == expected)
    }
  }
}
