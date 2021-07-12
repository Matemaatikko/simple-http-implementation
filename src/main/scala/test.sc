import jdk.nashorn.internal.parser.JSONParser
import jdk.nashorn.internal.runtime.QuotedStringTokenizer
import json._

val example =
  """
    |{
    |  "name" : "Jussi Meikälainen",
    |  "tosi": true,
    |  "epat\"osi": false,
    |  "lista": ["asd", 123.5, {"obj": 1}],
    |  "obscj\u1234": {"dassd": "as\tdtg", "val": true, "gg": []}
    | }
    |""".stripMargin


//example.toJson

val b: Object = "AS"
val c: Object = Integer(1)

import scala.quoted._

type Comb = String | Int

def f[T: Type](using Quotes): T =
  import quotes.reflect.*
  Type.of[T] match
    case a if a == Type.of[String] => "asd".asInstanceOf[T]
    case a if a == Type.of[Int] => 123.asInstanceOf[T]

f[String]
