package json

trait JsValue

case class JsObject(values: Seq[(String, JsValue)]) extends JsValue
case class JsArray(value: Seq[JsValue]) extends JsValue
case class JsString(value: String) extends JsValue
case class JsNumber(value: BigDecimal) extends JsValue

trait JsBoolean extends JsValue
case object JsTrue extends JsBoolean
case object JsFalse extends JsBoolean

case object JsNull extends JsValue
