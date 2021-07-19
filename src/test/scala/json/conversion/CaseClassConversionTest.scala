package json.conversion


import json._
import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class CaseClassConversionTest extends AnyFlatSpec with should.Matchers {

  object Conv extends ValueConversions
  import Conv.given

  object Conv1 extends CollectionConversion
  import Conv1.given

  object Conv2 extends CaseClassConversion
  import Conv2._

  def n(i: Int) = JsNumber(BigDecimal(i))
  def s(str: String) = JsString(str)

  case class A(a: String, b: Seq[Int], c: Long)
  case class B(a: String, b: Int, c: Char, d: A, e: Seq[A])

  given Class[A] = classOf[A]
  given Class[B] = classOf[B]

  given JsonConversion[A] = jsonConversion3(A.apply)
  given JsonConversion[B] = jsonConversion5(B.apply)

  val conv1: JsonConversion[A] = summon[JsonConversion[A]]
  val conv2: JsonConversion[B] = summon[JsonConversion[B]]

  "CaseClassConversion" should "work for case class A" in {
    val value = A("asd", Seq(1, 2, 3), 5l)
    val json = JsObject(Seq("a" -> s("asd"), "b" -> JsArray(Seq(n(1), n(2), n(3))), "c" -> n(5)))
    assert(conv1.fromJson(json) == value)
    assert(conv1.toJson(value) == json)
  }

  "CaseClassConversion" should "work for case class B" in {
    val valueA = A("asd", Seq(1, 2, 3), 5l)
    val valueB = B("asda", 12, 'a', valueA, Seq(valueA, valueA))
    val jsonA = JsObject(Seq("a" -> s("asd"), "b" -> JsArray(Seq(n(1), n(2), n(3))), "c" -> n(5)))
    val jsonB = JsObject(Seq("a" -> s("asda"), "b" -> n(12), "c" -> s("a"), "d" -> jsonA, "e" -> JsArray(Seq(jsonA, jsonA))))
    assert(conv2.fromJson(jsonB) == valueB)
    assert(conv2.toJson(valueB) == jsonB)
  }

}
