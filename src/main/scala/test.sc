import jdk.nashorn.internal.parser.JSONParser
import jdk.nashorn.internal.runtime.QuotedStringTokenizer
import json._

import scala.annotation.Annotation

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

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) / 1e+6  + "ms")
  result
}

case class Test(c: Long)
case class Test2(c: Long, sf: String)

given Class[Test] = classOf[Test]
given Class[Test2] = classOf[Test2]

import CaseClassConversion._
import ValueConversions.{given, *}


val conversion = jsonConversion1(Test.apply)
conversion.toJson(Test(3l))


val conversion2 = jsonConversion2(Test2.apply)
val jsoasdn = time{conversion2.toJson(Test2(3l, "ASXFgD"))}

val fields = time{
  classOf[Test2].getDeclaredFields
}

time{
  fields.head.setAccessible(true)
  fields.head.get(Test2(3l, "ASD"))
}


conversion2.fromJson(jsoasdn)


enum Planet:
  case Earth, Saturnus, Pluto

case class Wrap(a: String)
case class Simple(a: String, b: Int, c: Long, d: Wrap)
case class SimpleWithEnum(a: String, b: Int, d: Wrap, pl: Planet, s: Seq[String])


given Class[Wrap] = classOf[Wrap]
given Class[Simple] = classOf[Simple]

given JsonConversion[Wrap] = jsonConversion1(Wrap.apply)
val conversion4 = jsonConversion4(Simple.apply)


val simple = Simple("SDF", 213, 534l, Wrap("DSFSDF"))
val simple2 = Simple("SDFasd", 213, 534l, Wrap("DSFSDF"))

time(conversion4.toJson(simple))
time(ReflexConversion.toJson(simple))
(0 until 100).foreach(_ => time(ReflexConversion.toJson(simple2)))

val asdf = SimpleWithEnum("ASD", 214, Wrap("DF"), Planet.Pluto, Seq("ASDF", "fdf"))
time(ReflexConversion.toJson(asdf))

("asd", "asd", "dsa").toList.foreach(println(_))
