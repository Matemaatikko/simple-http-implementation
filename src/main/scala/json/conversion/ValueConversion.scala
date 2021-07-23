package json.conversion

import json._

trait JsonConversion[A] {
  def toJson(a: A): JsValue
  def fromJson(jsValue: JsValue): A
}

def error(jsValue: JsValue, _type: String) = throw new Exception(s"Failed to convert jsvalue ${jsValue} to ${_type}.")

trait ValueConversions {

  given JsonConversion[String] with
    def toJson(a: String) = JsString(a)
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsString(value) => value
      case _ => error(jsValue, "String")
    }

  given JsonConversion[Boolean] with
    def toJson(a: Boolean) = if(a) JsTrue else JsFalse
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsTrue => true
      case JsFalse => false
      case _ => error(jsValue, "Boolean")
    }

  given JsonConversion[Int] with
    def toJson(a: Int) = JsNumber(BigDecimal(a))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsNumber(value) if value.isValidInt => value.toIntExact
      case _ => error(jsValue, "Int")
    }

  given JsonConversion[Long] with
    def toJson(a: Long) = JsNumber(BigDecimal(a))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsNumber(value) if value.isValidLong => value.toLongExact
      case _ => error(jsValue, "Long")
    }

  given JsonConversion[Float] with
    def toJson(a: Float) = JsNumber(BigDecimal(a))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsNumber(value) => value.toFloat
      case _ => error(jsValue, "Float")
    }

  given JsonConversion[Double] with
    def toJson(a: Double) = JsNumber(BigDecimal(a))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsNumber(value) => value.toDouble
      case _ => error(jsValue, "Double")
    }

  given JsonConversion[BigInt] with
    def toJson(a: BigInt) = JsNumber(BigDecimal(a))
    def fromJson(jsValue: JsValue) =  jsValue match {
      case JsNumber(value) => value.toBigInt
      case _ => error(jsValue, "BigInt")
    }

  given JsonConversion[BigDecimal] with
    def toJson(a: BigDecimal) = JsNumber(a)
    def fromJson(jsValue: JsValue) =  jsValue match {
      case JsNumber (value) => value
      case _ => error (jsValue, "BigDecimal")
    }

  given JsonConversion[Byte] with
    def toJson(a: Byte) = JsNumber(BigDecimal(a))
    def fromJson(jsValue: JsValue) =  jsValue match {
      case JsNumber (value) if value.isValidByte => value.toByteExact
      case _ => error (jsValue, "Byte")
    }

  given JsonConversion[Short] with
    def toJson(a: Short) = JsNumber(BigDecimal(a))
    def fromJson(jsValue: JsValue) =  jsValue match {
      case JsNumber (value) if value.isValidShort => value.toShortExact
      case _ => error (jsValue, "Short")
    }

  given JsonConversion[Char] with
    def toJson(a: Char) = JsString(a.toString)
    def fromJson(jsValue: JsValue) =  jsValue match {
      case JsString(value) if value.length == 1 => value.charAt(0)
      case _ => error (jsValue, "Char")
    }

  given JsonConversion[Unit] with
    def toJson(a: Unit) = JsNull
    def fromJson(jsValue: JsValue) =  jsValue match {
      case JsNull => ()
      case _ => error (jsValue, "Unit")
    }
}




