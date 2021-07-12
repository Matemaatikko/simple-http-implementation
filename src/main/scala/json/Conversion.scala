package json


trait FromJson[A] {
  def apply(jsValue: JsValue): A
}

object ConvertToJson {

  type Combined = String | Boolean | Int | Long | Short | Double | BigInt | BigDecimal

  given apply1: (Combined => JsValue) = apply(_: Combined)
  given apply2[A](using to: A => JsValue):  (List[A] => JsValue) = apply(_: List[A])

  def apply(value: Combined): JsValue =
    value match {
      case a: String => JsString(a)
      case a: Boolean => if(a) JsTrue else JsFalse
      case a: Int => JsNumber(BigDecimal(a))
      case a: Long => JsNumber(BigDecimal(a))
      case a: Float => JsNumber(BigDecimal(a))
      case a: Double => JsNumber(BigDecimal(a))
      case a: BigInt => JsNumber(BigDecimal(a))
      case a: BigDecimal => JsNumber(a)
    }

  def apply[A](value: List[A])(using to: A => JsValue): JsValue = JsArray(value.map(to(_)))

}

private def error(jsValue: JsValue, _type: String) = throw new Exception(s"Failed to convert jsvalue ${jsValue} to ${_type}.")

object ConvertFromJson {
  type Convert[A] = JsValue => A

  val convertToString: Convert[String] = jsValue => jsValue match {
    case JsString(value) => value
    case _ => error(jsValue, "String")
  }
  val toBoolean: Convert[Boolean] = jsValue => jsValue match {
    case JsTrue => true
    case JsFalse => false
    case _ => error(jsValue, "Boolean")
  }
  val toInt: Convert[Int] = jsValue =>  jsValue match {
    case JsNumber(value) if value.isValidInt => value.toIntExact
    case _ => error(jsValue, "Int")
  }
  val toLong: Convert[Long] = jsValue =>  jsValue match {
    case JsNumber(value) if value.isValidLong => value.toLongExact
    case _ => error(jsValue, "Long")
  }
  val toFloat: Convert[Float] = jsValue =>  jsValue match {
    case JsNumber(value) => value.toFloat
    case _ => error(jsValue, "Float")
  }
  val toDouble: Convert[Double] = jsValue =>  jsValue match {
    case JsNumber(value) => value.toDouble
    case _ => error(jsValue, "Double")
  }
  val toBigInt: Convert[BigInt] = jsValue =>  jsValue match {
    case JsNumber(value) => value.toBigInt
    case _ => error(jsValue, "BigInt")
  }
  val toBigDecimal: Convert[BigDecimal] = jsValue =>  jsValue match {
    case JsNumber(value) => value
    case _ => error(jsValue, "BigDecimal")
  }
}




