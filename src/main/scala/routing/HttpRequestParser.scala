package routing

import scala.annotation.tailrec

case class HttpRequestParsingException(message: String) extends Exception

object ParseHttpRequest {
  def apply(str: String): HttpRequest =
    try
      new HttpRequestParser(str.iterator).parseRequest
    catch
      case a: HttpRequestParsingException => 
        throw a
      case a: Exception =>
        throw new HttpRequestParsingException(a.getMessage())
}

class HttpRequestParser(stream: Iterator[Char]) {

  var currentCharacter: Option[Char] = Some(' ')

  def peek: Char =
    currentCharacter.getOrElse(throw new HttpRequestParsingException("Failed to retreive a character from empty stream!"))

  def consume: Char =
    val last = peek
    if (stream.hasNext) currentCharacter = Some(stream.next)
    else currentCharacter = None
    last

  def skip(value: Char, error: String = ""): Unit =
    if peek != value then throw new HttpRequestParsingException(error + peek)
    consume

  def skipWhiteSpaces =
    while (currentCharacter.exists(_.isWhitespace)) consume

  def skipWhiteSpacesUnlessLineBreak =
    while (currentCharacter.exists(a => a.isWhitespace && a != '\n'))  consume

  def collectUntil(condition: => Boolean): String =
    @tailrec
    def iter(result: String): String =
      if(!condition) iter(result + consume)
      else result
    iter("")

  def doUntil[A](fun: => A, condition: => Boolean): Seq[A] =
    @tailrec
    def iter(result: Seq[A]): Seq[A] =
      if(!condition) iter(result :+ fun)
      else result
    iter(Nil)

  def isLineEnd =
    currentCharacter.map(_ == '\n').getOrElse(true)

  def parseRequest: HttpRequest =
    skipWhiteSpaces
    HttpRequest(
      parseMethod,
      parsePath,
      parseVersion,
      parseHeaders,
      parseBody
    )

  def parseMethod: HttpMethod =
    val method = collectUntil(peek == ' ')
    HttpMethod.valueOf(method)

  def parsePath: Path =
    skip(' ')
    if(peek == '/') Path.Route(collectUntil(peek == ' '))
    else Path.Url(collectUntil(peek == ' '))

  def parseVersion: HttpVersion =
    val version = collectUntil(isLineEnd)
    version.trim.toUpperCase match {
      case a if a == "HTTP/1.1" => HttpVersion.`Http/1.1`
      case a if a == "HTTP/1.0" => HttpVersion.`Http/1.0`
      case a          => throw new HttpRequestParsingException(s"Unknown http version: ${a}")
    }

  def parseHeaders: Seq[Header] =
    if(currentCharacter.isEmpty) then Nil
    else
      skip('\n',"Invalid line end. Character: ")
      doUntil(parseHeader, isLineEnd)

  def parseHeader: Header =
    val name = collectUntil(peek == ':')
    skip(':')
    val value = collectUntil(isLineEnd)
    currentCharacter.foreach(_ => skip('\n'))
    skipWhiteSpacesUnlessLineBreak
    Header(name.trim.toLowerCase, value.trim.toLowerCase)


  def parseBody: Option[String] =
    currentCharacter.map{_ =>
      skip('\n', "Missing empty line between body and headers. Character: ")
      collectUntil(currentCharacter.isEmpty)
    }
}