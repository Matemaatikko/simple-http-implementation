package json

import scala.annotation.tailrec
import scala.collection.mutable.Buffer


extension (str: String)
  def toJson: JsValue = ToJson(str)

object ToJson {
  def apply(str: String): JsValue =  new JsonParser(str.iterator).parseJsonValue
}


class JsonParser(stream: Iterator[Char]) {

  extension (char: Char)
    def isHexDigit = char.isDigit || Array('a', 'b', 'c', 'd', 'e', 'f', 'g').contains(char)

  var currentCharacter: Option[Char] = Some(' ')

  def peek: Char =
    currentCharacter.getOrElse(throw new Exception("Failed to retreive a character from empty stream!"))

  def consume: Char =
    val last = peek
    if(stream.hasNext) currentCharacter = Some(stream.next)
    else currentCharacter = None
    last

  def skip(value: Char): Unit =
    assert(peek == value)
    consume

  def skipWhiteSpaces =
    while(currentCharacter.exists(_.isWhitespace)) consume

  def collect(times: Int, condition: Char => Boolean): String =
    (0 until times).map(_ =>
      assert(condition(peek))
      consume
    ).mkString

  def parseJsonValue: JsValue =
    skipWhiteSpaces
    val result = peek match {
      case '{'  => parseObject
      case '"'  => JsString(parseString)
      case 't'  => parseTrue
      case 'f'  => parseFalse
      case 'n'  => parseNull
      case '['  => parseArray
      case '-'  => parseNumber
      case a if a.isDigit => parseNumber
    }
    skipWhiteSpaces
    result

  //JsObject

  def parseObject: JsObject =
    skip('{')
    val list = parseParamList
    skip('}')
    JsObject(list)

  def parseParamList: List[(String, JsValue)] =
    skipWhiteSpaces
    peek match {
      case '}' => Nil
      case '"' => parseObjectParam :: parseParamList
      case ',' =>
        consume
        parseObjectParam :: parseParamList
    }

  def parseObjectParam: (String, JsValue) =
    skipWhiteSpaces
    val param = parseString
    skipWhiteSpaces
    skip(':')
    val value = parseJsonValue
    (param, value)

  //JsString

  def parseString: String =
    skip('\"')
    val result = parseStringValue
    skip('\"')
    result


  private def parseStringValue: String =
    @tailrec
    def iter(result: String): String =
      if(peek == '"') result
      else iter(result + parseCharacter)
    iter("")


  def parseCharacter: String =
    peek match {
      case '\\' =>
        consume
        parseSpecialCharacter
      case '\"' => ""
      case _ => consume.toString
    }

  def parseSpecialCharacter: String =
    peek match {
      case '"'  => consume; "\""
      case '\\' => consume; "\\"
      case '/'  => consume; "/"
      case 'b'  => consume; "\b"
      case 'f'  => consume; "\f"
      case 'n'  => consume; "\n"
      case 'r'  => consume; "\r"
      case 't'  => consume; "\t"
      case 'u'  =>
        consume
        val values = collect(4, _.isHexDigit)
        values.toInt.toChar.toString
    }

  // Constants

  def parseTrue: JsBoolean =
    skip('t')
    skip('r')
    skip('u')
    skip('e')
    JsTrue

  def parseFalse: JsBoolean =
    skip('f')
    skip('a')
    skip('l')
    skip('s')
    skip('e')
    JsFalse

  def parseNull: JsValue =
    skip('n')
    skip('u')
    skip('l')
    skip('l')
    JsNull

  //JsArray

  def parseArray: JsArray =
    skip('[')
    skipWhiteSpaces
    val elements = parseList
    skip(']')
    JsArray(elements)


  def parseList: List[JsValue] =
    @tailrec
    def iter(result: List[JsValue]): List[JsValue] =
      peek match {
        case ']' => result
        case ',' => consume; iter(result)
        case _ =>   iter(result :+ parseJsonValue)
      }
    end iter
    iter(Nil)

  //JsNumber

  def parseNumber: JsNumber =
    JsNumber(parseNumberValue)


  def parseNumberValue: BigDecimal =
    peek match {
      case '-' => consume; - parseNumberValue
      case '0' => consume; parseFraction
      case a if a.isDigit => BigDecimal(parseInteger) + parseFraction
    }

  def parseInteger: String =
    @tailrec
    def iter(result: String): String =
      if(peek.isDigit) iter(result + consume.toString)
      else result
    iter("")

  def parseFraction: BigDecimal =
    peek match {
      case '.'        => consume; BigDecimal("0." + parseInteger)
      case 'e' | 'E'  => consume; BigDecimal("1e" + parseSign + parseInteger)
      case _          => BigDecimal(0)
    }

  def parseSign: String =
    peek match {
      case '+' => consume; "+"
      case '-' => consume; "-"
      case  _  => "+"
    }

}



