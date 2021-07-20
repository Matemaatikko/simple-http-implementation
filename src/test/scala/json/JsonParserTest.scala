package json

import org.scalatest._
import flatspec._
import matchers._

class JsonParserTest extends AnyFlatSpec with should.Matchers {

  val example =
    """
      |{
      |  "name" : "Jussi Meikälainen \n Something",
      |  "tosi": true,
      |  "epat\"osi": false,
      |  "lista": ["asd", 123.5, {"obj": -0e-10}],
      |  "obscj\u1234": {"dassd": "as\tdtg", "val": true, "gg": []}
      | }
      |""".stripMargin

  "JsParser" should "parse given string" in {
    try
      ToJson(example)
      assert(true)
    catch
      case e: Exception => assert(false)
  }

  it should "parse string properly" in {
    val input = "\"safg\u1234\t\\/\""
    val output = JsString("safg\u1234\t/")
    assert(ToJson(input) == output)
  }

  it should "parse numbers properly" in {
    assert(ToJson("123") == JsNumber(BigDecimal("123")))
    assert(ToJson("-123") == JsNumber(BigDecimal("-123")))
    assert(ToJson("0.1234") == JsNumber(BigDecimal("0.1234")))
    assert(ToJson("0e10") == JsNumber(BigDecimal("0")))
    assert(ToJson("-1e-10") == JsNumber(BigDecimal("-1e-10")))
    assert(ToJson("1E+10") == JsNumber(BigDecimal("1e+10")))
  }

  it should "parse boolean values properly" in {
    assert(ToJson("true") == JsTrue)
    assert(ToJson("false") == JsFalse)
  }

  it should "parse null properly" in {
    assert(ToJson("null") == JsNull)
  }

  it should "parse array properly" in {
    assert(ToJson("[1, 2, 3]") == JsArray(Seq(JsNumber(BigDecimal(1)), JsNumber(BigDecimal(2)), JsNumber(BigDecimal(3)))))
  }

  it should "parse object properly" in {
    val objectString = "{\"a\": 1, \"b\": [], \"c\": {\"d\": 1}}"
    val result = JsObject(Seq("a" -> JsNumber(BigDecimal(1)), "b" -> JsArray(Seq()), "c" -> JsObject(Seq("d" -> JsNumber(BigDecimal(1))))))
    assert(ToJson(objectString) == result)
  }

}