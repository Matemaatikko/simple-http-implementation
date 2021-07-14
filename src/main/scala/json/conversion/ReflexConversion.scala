package json.conversion

import json._

object ReflexConversion {

  def toJson(any: Any): JsValue =
    val c = any.getClass
    any match {
      case a: String     => JsString(any.asInstanceOf[String])
      case a: Int        => JsNumber(BigDecimal(any.asInstanceOf[Int]))
      case a: Long       => JsNumber(BigDecimal(any.asInstanceOf[Long]))
      case a: Short      => JsNumber(BigDecimal(any.asInstanceOf[Short]))
      case a: Byte       => JsNumber(BigDecimal(any.asInstanceOf[Byte]))
      case a: BigInt     => JsNumber(BigDecimal(any.asInstanceOf[BigInt]))
      case a: Float      => JsNumber(BigDecimal(any.asInstanceOf[Float]))
      case a: Double     => JsNumber(BigDecimal(any.asInstanceOf[Double]))
      case a: BigDecimal => JsNumber(any.asInstanceOf[Double])
      case a: Char       => JsString(any.asInstanceOf[Char].toString)
      case a: Array[_]   => JsArray(a.map(toJson(_)))
      case a: List[_]    => JsArray(a.map(toJson(_)))
      case a: Seq[_]     => JsArray(a.map(toJson(_)))
      case a: Set[_]     => JsArray(a.toSeq.map(toJson(_)))
      case a: Tuple2[_, _]   => JsArray(a.toList.map(toJson(_)))
      case a: Tuple3[_, _, _]   => JsArray(a.toList.map(toJson(_)))
      case a: Tuple4[_, _, _, _]   => JsArray(a.toList.map(toJson(_)))
      case a: Tuple5[_, _, _, _, _]   => JsArray(a.toList.map(toJson(_)))
      case a: Tuple6[_, _, _, _, _, _]   => JsArray(a.toList.map(toJson(_)))
      case a: Option[_]   => a.map(toJson(_)).getOrElse(JsNull)
      case a: Map[_, _]   => JsArray(a.toSeq.map(toJson(_)))
      case a if c.getDeclaredFields.map(_.getName).contains("$name$1") =>
        val field = c.getDeclaredField("$name$1")
        field.setAccessible(true)
        JsString(field.get(any).asInstanceOf[String])
      case a if c.getDeclaredFields.nonEmpty     => classToJson(any)
      case _                                     => throw new Exception()
    }

  private def classToJson(value: Any): JsObject =
    val c = value.getClass
    JsObject(
      c.getDeclaredFields.map(field => {
        field.setAccessible(true)
        field.getName -> toJson(field.get(value))
      })
    )


}
