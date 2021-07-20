package routing

import org.scalatest._
import flatspec._
import matchers._

class HttpRequestParserTest extends AnyFlatSpec with should.Matchers {

  val simpleRequest =
    """
      |POST / HTTP/1.1
      |HOST: localhost:1080
      |content-type: text/plain;charset=UTF-8
      |content-length: 14
      |
      |{"value": 123}""".stripMargin

  "HttpRequestParser" should "work" in {
    val request = ParseHttpRequest(simpleRequest)
    val headers = Seq(
      Header("host", "localhost:1080"),
      Header("content-type", "text/plain;charset=utf-8"),
      Header("content-length", "14")
    )
    val result = HttpRequest(HttpMethod.POST, Path.Route("/"), HttpVersion.`Http/1.1`, headers, Some("{\"value\": 123}"))
    assert(request == result)
  }

}
