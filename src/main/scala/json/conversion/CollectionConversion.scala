package json.conversion

import json._

import scala.reflect.ClassTag

trait CollectionConversion {

  given seqConv[A](using conv: JsonConversion[A]): JsonConversion[Seq[A]] with
    def toJson(a: Seq[A]) = JsArray(a.map(conv.toJson(_)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(value) => value.map(conv.fromJson(_))
      case _ => error(jsValue, "Seq")
    }

  given listConv[A](using conv: JsonConversion[A]): JsonConversion[List[A]] with
    def toJson(a: List[A]) = JsArray(a.map(conv.toJson(_)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(value) => value.map(conv.fromJson(_)).toList
      case _ => error(jsValue, "Seq")
    }

  given setConv[A](using conv: JsonConversion[A]): JsonConversion[Set[A]] with
    def toJson(a: Set[A]) = JsArray(a.toSeq.map(conv.toJson(_)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(value) => value.map(conv.fromJson(_)).toSet
      case _ => error(jsValue, "Seq")
    }

  given arrayConv[A: ClassTag](using conv: JsonConversion[A]): JsonConversion[Array[A]] with
    def toJson(a: Array[A]) = JsArray(a.map(conv.toJson(_)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(value) => value.map(conv.fromJson(_)).toArray
      case _ => error(jsValue, "Array")
    }

  given mapConv[A, B](using conv: JsonConversion[(A, B)]): JsonConversion[Map[A, B]] with
    def toJson(a: Map[A, B]) = JsArray(a.toSeq.map(conv.toJson(_)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(value) => value.map(conv.fromJson(_)).toMap
      case _ => error(jsValue, "Map")
    }

  given optConv[A](using conv: JsonConversion[A]): JsonConversion[Option[A]] with
    def toJson(a: Option[A]) = a.map(conv.toJson(_)).getOrElse(JsNull)
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(list) if list == Nil      => None
      case JsArray(list) if list.length == 1 => Some(conv.fromJson(list.head))
      case JsNull                            => None
      case a: JsValue                        => Some(conv.fromJson(a))
      case _ => error(jsValue, "Option")
    }

  given tupleConv2[A1, A2](using conv1: JsonConversion[A1], conv2: JsonConversion[A2]): JsonConversion[(A1, A2)] with
    def toJson(a: (A1, A2)) = JsArray(Seq(conv1.toJson(a._1), conv2.toJson(a._2)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(list) if list.length == 2 => (conv1.fromJson(list(0)), conv2.fromJson(list(1)))
      case _ => error(jsValue, "Tuple3")
    }

  given tupleConv3[A1, A2, A3](using conv1: JsonConversion[A1], conv2: JsonConversion[A2],
                                     conv3: JsonConversion[A3]): JsonConversion[(A1, A2, A3)] with
    def toJson(a: (A1, A2, A3)) = JsArray(Seq(conv1.toJson(a._1), conv2.toJson(a._2), conv3.toJson(a._3)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(list) if list.length == 3 => (conv1.fromJson(list(0)), conv2.fromJson(list(1)), conv3.fromJson(list(2)))
      case _ => error(jsValue, "Tuple4")
    }

  given tupleConv4[A1, A2, A3, A4](using conv1: JsonConversion[A1], conv2: JsonConversion[A2],
                                   conv3: JsonConversion[A3], conv4: JsonConversion[A4]): JsonConversion[(A1, A2, A3, A4)] with
    def toJson(a: (A1, A2, A3, A4)) = JsArray(Seq(conv1.toJson(a._1), conv2.toJson(a._2), conv3.toJson(a._3), conv4.toJson(a._4)))
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsArray(list) if list.length == 4 => (conv1.fromJson(list(0)), conv2.fromJson(list(1)), conv3.fromJson(list(2)), conv4.fromJson(list(3)))
      case _ => error(jsValue, "Tuple2")
    }


  def enumConversion[A](valueOf: String => A) = new JsonConversion[A] {
    def toJson(a: A) = JsString(a.toString)
    def fromJson(jsValue: JsValue) = jsValue match {
      case JsString(value) => valueOf(value)
      case _ => error(jsValue, "Enum")
    }
  }

}
