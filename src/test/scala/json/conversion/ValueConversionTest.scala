package json.conversion

import json._
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class ValueConversionTest extends AnyFlatSpec with should.Matchers {

  object Conv extends ValueConversions
  import Conv.given

  def test[A](value: A, json: JsValue)(using conv: JsonConversion[A]) =
    assert(conv.fromJson(json) == value)
    assert(conv.toJson(value) == json)

  "IntConversion" should "work" in {
    test(123, JsNumber(BigDecimal(123)))
  }

  "LongConversion" should "work" in {
    test(123l, JsNumber(BigDecimal(123l)))
  }

  "BigIntConversion" should "work" in {
    test(BigInt(123), JsNumber(BigDecimal(123)))
  }

  "StringConversion" should "work" in {
    test("string", JsString("string"))
  }

  "BooleanConversion" should "work" in {
    test(true, JsTrue)
    test(false, JsFalse)
  }

  "FloatConversion" should "work" in {
    test(0.123f, JsNumber(BigDecimal(0.123f)))
  }

  "DoubleConversion" should "work" in {
    test(0.123d, JsNumber(BigDecimal(0.123d)))
  }

  "ShortConversion" should "work" in {
    test(12.toShort, JsNumber(BigDecimal(12)))
  }

  "ByteConversion" should "work" in {
    test(12.toByte, JsNumber(BigDecimal(12)))
  }

  "BigDecimalConversion" should "work" in {
    test(BigDecimal(12), JsNumber(BigDecimal(12)))
  }

  "CharConversion" should "work" in {
    test('c', JsString("c"))
  }

}
