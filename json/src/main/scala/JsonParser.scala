import com.jpmc.parsecs.Parser
import com.jpmc.parsecs.Parser._
import com.jpmc.json.Json._
object JsonParser {
  def jStringStr: Parser[String] = quote *> jString <* quote
  def jNumber: Parser[Number] = number.map(Number)
  def jBoolean: Parser[Boolean] = boolean.map(b => Boolean(b))
  def jString: Parser[String] = Parser.alphaNumeric.rep0.map(_.mkString).map(String)
  def quote: Parser[Char] = Parser.char('\"')
  // think about how you would implement a parser for {...}
  // opening char something in between then closing char
}
