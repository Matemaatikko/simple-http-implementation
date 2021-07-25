import jdk.nashorn.internal.parser.JSONParser
import jdk.nashorn.internal.runtime.QuotedStringTokenizer
import json._
import json.conversion.{CaseClassConversionGeneration, JsonConversion, ReflectionConversion}

import scala.annotation.Annotation


//example.toJson

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) / 1e+6  + "ms")
  result
}
import routing.HttpVersion._

println(`Http/1.1`)

import routing._

val response = HttpResponse(
  HttpVersion.`Http/1.1`,
  StatusCode.InternalServerError,
  Map("Content-type"-> "application/json", "Content-length" -> "5"),
  Some("12345")
)

response.httpString