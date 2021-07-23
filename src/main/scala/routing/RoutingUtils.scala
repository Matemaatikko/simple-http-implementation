package routing

import json.conversion.JsonConversion
import json.{JsonPrinter, ToJson}
import routing.RoutingUtils.Fun


case object MissingBodyException extends Exception

object RoutingUtils {

  import routing._

  type Fun[A, B, N <: Int] = (A, Params[N]) => (StatusCode, B)
  type Fun1[B, N <: Int] = Params[N] => (StatusCode, B)

  extension[A, B, N <: Int](path: RoutePath[N])
    def apply(tuple: (HttpMethod, Fun[A, B, N]))(using conv1: JsonConversion[A], conv2: JsonConversion[B]) =
      Seq(Route[N](path.reverse, tuple._1, resolveHander(tuple._2)))

  extension[B, N <: Int](path: RoutePath[N])
    def apply(tuple: (HttpMethod, Fun1[B, N]))(using conv2: JsonConversion[B]) =
      Seq(Route[N](path.reverse, tuple._1, resolveHander1(tuple._2)))

  extension[A, B, N <: Int](routes: Seq[Route[N]])
    def ~(tuple: (HttpMethod, Fun[A, B, N]))(using conv1: JsonConversion[A], conv2: JsonConversion[B]): Seq[Route[N]] = routes :+ Route[N](routes.head.path, tuple._1, resolveHander(tuple._2))

  extension[B, N <: Int](routes: Seq[Route[N]])
    def ~(tuple: (HttpMethod, Fun1[B, N]))(using conv2: JsonConversion[B]): Seq[Route[N]] = routes :+ Route[N](routes.head.path, tuple._1, resolveHander1(tuple._2))

  private def resolveHander[A, B, N <: Int](fun: Fun[A, B, N])
                                 (using conv1: JsonConversion[A], conv2: JsonConversion[B]): HttpHandler[N] =
    args =>
      val (request, params) = args
      val requestBody = request.body.getOrElse(throw MissingBodyException)
      val json = ToJson(requestBody)
      val (statusCode, result) = fun(conv1.fromJson(json), params)
      val resultJson = conv2.toJson(result)
      val responseBody = if result == () then None else Some(JsonPrinter.print(resultJson))
      generateResponse(request.version, statusCode, responseBody)

  private def resolveHander1[B, N <: Int](fun: Fun1[B, N])
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

  extension[A, B](fun: A => (StatusCode, B))
    def convert: (A, Params[0]) => (StatusCode, B) = (a, params) => fun(a)

  extension[A, B](fun: (A, String) => (StatusCode, B))
    def convert: (A, Params[1]) => (StatusCode, B) = (a, params) => fun(a, params.get(0))

  extension[A, B](fun: (A, String, String) => (StatusCode, B))
    def convert: (A, Params[2]) => (StatusCode, B) = (a, params) => fun(a, params.get(0), params.get(1))

  extension[A, B](fun: (A, String, String, String) => (StatusCode, B))
    def convert: (A, Params[3]) => (StatusCode, B) = (a, params) => fun(a, params.get(0), params.get(1), params.get(2))


}
