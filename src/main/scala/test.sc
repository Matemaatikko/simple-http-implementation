import jdk.nashorn.internal.parser.JSONParser
import jdk.nashorn.internal.runtime.QuotedStringTokenizer
import json._
import json.conversion.{CaseClassConversionGeneration, JsonConversion, ReflexConversion}

import scala.annotation.Annotation


//example.toJson

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) / 1e+6  + "ms")
  result
}


val simpleRequest =
  """
    |POST / HTTP/1.1
    |HOST: localhost:1080
    |content-type: text/plain;charset=UTF-8
    |content-length: 14
    |
    |{\"value\": 123}""".stripMargin
val simpleRequest2 =
  s"""
    |POST / HTTP/1.1
    |HOST: localhost:1080
    |content-type: text/plain;charset=UTF-8
    |content-length: 14${"\n"}
    |{\"value\": 123}""".stripMargin

println(simpleRequest)
println(simpleRequest2)
simpleRequest == simpleRequest2
simpleRequest.foreach(char => println("Char: "+ char))
simpleRequest2.foreach(char => println("Char: "+ char))