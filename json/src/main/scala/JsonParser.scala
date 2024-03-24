import com.jpmc.json.{Json, JsonSeparators}
import com.jpmc.parsecs.Parser
import com.jpmc.parsecs.Parser._
import com.jpmc.json.Json._
import CharParsers._
object JsonParser {
  def jStringStr: Parser[String] = quote *> Parser.alphaNumeric.rep0.map(_.mkString).map(String) <* quote
  def jNumber: Parser[Number] = number.map(Number)
  def jBoolean: Parser[Boolean] = boolean.map(b => Boolean(b))
  def jNull: Parser[Json.Null.type] = string("null").map(_ => Null)
  def jAny: Parser[Json] = jNumber | jBoolean | jStringStr | jNull | jArray | jObject
  def key: Parser[java.lang.String] = quote *> Parser.alphaNumeric.rep0.map(_.mkString) <* quote
  def keyValue: Parser[List[(java.lang.String, Json)]] = ((key <* colonSep) ~ jAny).repSep0(JsonSeparators.commaSep)
  def jArray: Parser[Array] = (openSquare *> jAny.repSep0(JsonSeparators.commaSep) <* closeSquare).map(Array)
  def jObject: Parser[Object] = openBrace *> keyValue.map(pairs => Json.Object(pairs.toMap)) <* closeBrace
}

object CharParsers {
  def quote: Parser[java.lang.String] = Parser.string(JsonSeparators.quotes)
  def colonSep: Parser[java.lang.String] = Parser.string(JsonSeparators.colon)
  def openBrace: Parser[java.lang.String] = Parser.string(JsonSeparators.openCurly)
  def closeBrace: Parser[java.lang.String] = Parser.string(JsonSeparators.closeCurly)
  def openSquare: Parser[java.lang.String] = Parser.string(JsonSeparators.openSquare)
  def closeSquare: Parser[java.lang.String] = Parser.string(JsonSeparators.closeSquare)
}