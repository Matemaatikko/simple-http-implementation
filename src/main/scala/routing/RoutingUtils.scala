package routing

import json.conversion.JsonConversion
import json.{JsonPrinter, ToJson}
import routing.RoutingUtils.Fun
import routing._

case object MissingBodyException extends Exception

/***
  * Helper-class for unpacking HttpRequest-information.
  */
object RoutingUtils {

  /***
    * Notation:
    *   A = Body-type
    *   B = Result-type
    */


  type Fun[A, B, N <: Int] = (A, Params[N]) => (StatusCode, B)
  type Fun1[B, N <: Int] = Params[N] => (StatusCode, B)

  // First route creators

  extension[A, B, N <: Int](path: RoutePath[N])
    def apply(tuple: (HttpMethod, Fun[A, B, N]))(using conv1: JsonConversion[A], conv2: JsonConversion[B]) =
      Seq(Route[N](path.reverse, tuple._1, resolveHandler(tuple._2)))

  extension[B, N <: Int](path: RoutePath[N])
    def apply(tuple: (HttpMethod, Fun1[B, N]))(using conv2: JsonConversion[B]) =
      Seq(Route[N](path.reverse, tuple._1, resolveHandler1(tuple._2)))

  // Additional route creators

  extension[A, B, N <: Int](routes: Seq[Route[N]])
    def ~(tuple: (HttpMethod, Fun[A, B, N]))(using conv1: JsonConversion[A], conv2: JsonConversion[B]): Seq[Route[N]] =
      routes :+ Route[N](routes.head.path, tuple._1, resolveHandler(tuple._2))

  extension[B, N <: Int](routes: Seq[Route[N]])
    def ~(tuple: (HttpMethod, Fun1[B, N]))(using conv2: JsonConversion[B]): Seq[Route[N]] =
      routes :+ Route[N](routes.head.path, tuple._1, resolveHandler1(tuple._2))

  //HttpHandler constructors

  private def resolveHandler[A, B, N <: Int](fun: Fun[A, B, N])
                                 (using conv1: JsonConversion[A], conv2: JsonConversion[B]): HttpHandler[N] =
    args =>
      val (request, params) = args
      val requestBody = request.body.getOrElse(throw MissingBodyException)
      val json = ToJson(requestBody)
      val (statusCode, result) = fun(conv1.fromJson(json), params)
      val resultJson = conv2.toJson(result)
      val responseBody = if result == () then None else Some(JsonPrinter.print(resultJson))
      generateResponse(request.version, statusCode, responseBody)

  private def resolveHandler1[B, N <: Int](fun: Fun1[B, N])
                                           (using conv2: JsonConversion[B]): HttpHandler[N] =
    args =>
      val (request, params) = args
      val (statusCode, result) = fun(params)
      val resultJson = conv2.toJson(result)
      val responseBody = if result == () then None else Some(JsonPrinter.print(resultJson))
      generateResponse(request.version, statusCode, responseBody)

  def generateResponse(version: HttpVersion, statusCode: StatusCode, body: Option[String]) =
    HttpResponse(
      version,
      statusCode,
      Map(
        "Content-type"->"application/json",
        "Content-length" -> body.map(_.length.toString).getOrElse("")
      ),
      body
    )

  //Conversions with body

  extension[A, B](fun: A => (StatusCode, B))
    def convert: (A, Params[0]) => (StatusCode, B) = (a, params) => fun(a)

  extension[A, B](fun: (A, String) => (StatusCode, B))
    def convert: (A, Params[1]) => (StatusCode, B) = (a, params) => fun(a, params.get(0))

  extension[A, B](fun: (A, String, String) => (StatusCode, B))
    def convert: (A, Params[2]) => (StatusCode, B) = (a, params) => fun(a, params.get(0), params.get(1))

  extension[A, B](fun: (A, String, String, String) => (StatusCode, B))
    def convert: (A, Params[3]) => (StatusCode, B) = (a, params) => fun(a, params.get(0), params.get(1), params.get(2))

  //Body-less conversions

  extension[B](fun:  () => (StatusCode, B))
    def convert: (Params[0]) => (StatusCode, B) = params => fun()

  extension[B](fun: (String) => (StatusCode, B))
    def convert: Params[1] => (StatusCode, B) = params => fun(params.get(0))

  extension[B](fun: (String, String) => (StatusCode, B))
    def convert: Params[2] => (StatusCode, B) = params => fun(params.get(0), params.get(1))

  extension[B](fun: (String, String, String) => (StatusCode, B))
    def convert: (Params[3]) => (StatusCode, B) = params => fun(params.get(0), params.get(1), params.get(2))

}
