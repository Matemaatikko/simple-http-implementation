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

