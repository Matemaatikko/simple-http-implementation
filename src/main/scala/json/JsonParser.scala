package json

import scala.annotation.tailrec
import scala.collection.mutable.Buffer

case class JsonParsingException(message: String) extends Exception

object ToJson {
  def apply(str: String): JsValue =
    try
      new JsonParser((str + " ").iterator).parseJsonValue
    catch
      case e: Exception => throw JsonParsingException(e.getMessage)
}

//Json-format used: ttps://www.json.org/json-en.html
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

  def skip(value: Char, error: String = ""): Unit =
    if peek != value then throw new Exception(error + peek)
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
      case _ => throw new Exception(s"Failed to parse json value. Invalid character: ${peek}")
    }
    skipWhiteSpaces
    result

  //JsObject

  def parseObject: JsObject =
    skip('{', "Parsing object. Expecting {. Having: ")
    val list = parseParamList
    skip('}', "Parsing object. Expecting }. Having: ")
    JsObject(list)

  def parseParamList: List[(String, JsValue)] =
    skipWhiteSpaces
    peek match {
      case '}' => Nil
      case '"' => parseObjectParam :: parseParamList
      case ',' =>
        consume
        parseObjectParam :: parseParamList
      case _ => throw new Exception(s"Failed to parse parameter list for object. Invalid character: ${peek}")
    }

  def parseObjectParam: (String, JsValue) =
    skipWhiteSpaces
    val param = parseString
    skipWhiteSpaces
    skip(':', "Parsing parameter list for object. Expecting :. Having: ")
    val value = parseJsonValue
    (param, value)

  //JsString

  def parseString: String =
    skip('\"', "String parsing: Start. Expecting \". Having: ")
    val result = parseStringValue
    skip('\"', "String parsing: End. Expecting \". Having: ")
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
      case _ => throw new Exception(s"Failed to parse special character: ${peek}")
    }

  // Constants

  def parseTrue: JsBoolean =
    skip('t', "Boolean parsing: true. Expecting: t. Having: ")
    skip('r', "Boolean parsing: true. Expecting: r. Having: ")
    skip('u', "Boolean parsing: true. Expecting: u. Having: ")
    skip('e', "Boolean parsing: true. Expecting: e. Having: ")
    JsTrue

  def parseFalse: JsBoolean =
    skip('f', "Boolean parsing: false. Expecting: f. Having: ")
    skip('a', "Boolean parsing: false. Expecting: a. Having: ")
    skip('l', "Boolean parsing: false. Expecting: l. Having: ")
    skip('s', "Boolean parsing: false. Expecting: s. Having: ")
    skip('e', "Boolean parsing: false. Expecting: e. Having: ")
    JsFalse

  def parseNull: JsValue =
    skip('n', "Null parsing. Expecting: n. Having: ")
    skip('u', "Null parsing. Expecting: u. Having: ")
    skip('l', "Null parsing. Expecting: l. Having: ")
    skip('l', "Null parsing. Expecting: l. Having: ")
    JsNull

  //JsArray

  def parseArray: JsArray =
    skip('[', "Array parsing. Expecting: [. Having: ")
    skipWhiteSpaces
    val elements = parseList
    skip(']', "Array parsing. Expecting: ]. Having: ")
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
      case '0' => consume; parseZero
      case a if a.isDigit => BigDecimal(parseInteger + parseFraction)
      case _ => throw new Exception(s"Failed to parse number. Invalid first digit: ${peek}")
    }

  def parseInteger: String =
    @tailrec
    def iter(result: String): String =
      if(peek.isDigit) iter(result + consume.toString)
      else result
    iter("")

  def parseZero: BigDecimal =
      peek match {
        case '.'        => consume; BigDecimal("0." + parseInteger)
        case 'e' | 'E'  => consume; parseSign; parseInteger; BigDecimal(0)
        case _          => BigDecimal(0)
      }

  def parseFraction: String =
    peek match {
      case '.'        => consume; "0." + parseInteger
      case 'e' | 'E'  => consume; "e" + parseSign + parseInteger
      case _          => ""
    }

  def parseSign: String =
    peek match {
      case '+' => consume; "+"
      case '-' => consume; "-"
      case a if a.isDigit  => "+"
      case _ => throw new Exception(s"Failed to parse exponent sign. Invalid exponent value: ${peek}")
    }

}



