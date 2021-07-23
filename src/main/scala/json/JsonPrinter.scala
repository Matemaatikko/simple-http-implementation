package json


//TODO add tests
//TODO add prettyPrint
object JsonPrinter {

  def print(jsValue: JsValue): String = jsValue match {
    case JsObject(values) => s"{${values.map((name, value) => s"\"$name\": ${print(value)}").mkString(", ")}}"
    case JsArray(list) => s"[${list.map(print(_)).mkString(", ")}]"
    case JsString(value) => s"\"$value\""
    case JsNumber(value) => value.toString()
    case JsTrue          => "true"
    case JsFalse         => "false"
    case JsNull          => "null"
  }
}
