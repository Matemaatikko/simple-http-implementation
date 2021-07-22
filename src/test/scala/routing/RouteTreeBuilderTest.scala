package routing

import org.scalatest._
import flatspec._
import matchers._
import routing.RoutePath.Root

class RouteTreeBuilderTest extends AnyFlatSpec with should.Matchers {

  import Params._
  val response =  HttpResponse(HttpVersion.`Http/1.0`, StatusCode.OK, Map(), None)

  def fun0(request: HttpRequest): HttpResponse = response
  def fun1(request: HttpRequest, str: String): HttpResponse = response

  type Fun0 = HttpRequest => HttpResponse
  type Fun1 = (HttpRequest, String) => HttpResponse

  given Conversion[Fun0, HttpHandler[0]] with
    def apply(fun: Fun0) = args => fun(args._1)

  given Conversion[Fun1, HttpHandler[1]] with
    def apply(fun: Fun1): HttpHandler[1] = args => fun(args._1, args._2.get(0))


  import VariableLike._
  import HttpMethod._
  import scala.language.implicitConversions

  val routes: Seq[Route[? <: Int]] =
    (Root / "root" / "test" / "values") (
      POST -> fun0,
      GET  -> fun0
    ) ++ (Root / "root" / "user") (
      POST -> fun0
    ) ++ (Root / "root" / "test" / "values" / "extra") (
      POST -> fun0
    ) ++ (Root / "root" / Variable / "new-stuff") (
      POST -> fun1
    )

  "RouteTreeBuilding" should "work" in {
    RouteTreeBuilder.build(routes)
  }



}
