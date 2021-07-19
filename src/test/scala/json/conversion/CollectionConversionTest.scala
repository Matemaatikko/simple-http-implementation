package json.conversion

import json._
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._


class CollectionConversionTest extends AnyFlatSpec with should.Matchers {

  object Conv extends ValueConversions
  import Conv.given

  object Conv1 extends CollectionConversion
  import Conv1.given


  def test[A](value: A, json: JsValue)(using conv: JsonConversion[A]) =
    assert(conv.fromJson(json) == value)
    assert(conv.toJson(value) == json)

  def n(i: Int) = JsNumber(BigDecimal(i))

  "SeqConversion" should "work" in {
    test(Seq(1, 2, 3), JsArray(Seq(n(1), n(2), n(3))))
  }

  "ListConversion" should "work" in {
    test(List(1, 2, 3), JsArray(Seq(n(1), n(2), n(3))))
  }

  "SetConversion" should "work" in {
    test(Set(1, 2, 3), JsArray(Seq(n(1), n(2), n(3))))
  }

  "ArrayConversion" should "work" in {
    val value = Array(1, 2, 3)
    val json = JsArray(Seq(n(1), n(2), n(3)))
    val conv = summon[JsonConversion[Array[Int]]]
    assert(conv.fromJson(json).toSeq == value.toSeq)
    assert(conv.toJson(value) == json)
  }

  "MapConversion" should "work" in {
    test(Map(1 -> "1", 2 -> "2"), JsArray(Seq(JsArray(Seq(n(1), JsString("1"))), JsArray(Seq(n(2), JsString("2"))))))
  }

  "OptConversion" should "work" in {
    test(Option(1), n(1))
    test[Option[Int]](None, JsNull)
  }

  "Tuple2Conversion" should "work" in {
    test(("as", 1), JsArray(Seq(JsString("as"), n(1))))
  }

  "Tuple3Conversion" should "work" in {
    test(("as", 1, 2), JsArray(Seq(JsString("as"), n(1), n(2))))
  }

  "Tuple4Conversion" should "work" in {
    test(("as", 1, 2, 3), JsArray(Seq(JsString("as"), n(1), n(2), n(3))))
  }

  "EnumConversion" should "work" in {

    enum Planet:
      case Pluto, Saturnus, Earth
    end Planet

    import Conv1._

    given JsonConversion[Planet] = enumConversion(Planet.valueOf)
    test(Planet.Pluto, JsString("Pluto"))
    test(Planet.Earth, JsString("Earth"))
  }


}
