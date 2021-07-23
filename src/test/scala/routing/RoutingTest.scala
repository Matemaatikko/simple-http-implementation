package routing

import org.scalatest._
import flatspec._
import matchers._
import routing.RoutePath.Root

class RoutingTest extends AnyFlatSpec with should.Matchers {

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

  val route: Route[0] = (Root / "root" / "test" / "values") (POST -> fun0).head
  val route2: Route[1] = (Root / "root" / Variable / "values") (POST -> fun1).head

  import routing._

  "Routing" should "match" in {
    val path = "root/test/values".split("/").toSeq
    assert(path.matching(route.path).nonEmpty)
  }

  it should "fail to match 1" in {
    val path = "root/test/asd".split("/").toSeq
    assert(path.matching(route.path).isEmpty)
  }

  it should "fail to match 2" in {
    val path = "asd/test/values".split("/").toSeq
    assert(path.matching(route.path).isEmpty)
  }

  it should "fail to match 3" in {
    val path = "root/test/values/something".split("/").toSeq
    assert(path.matching(route.path).isEmpty)
  }

  it should "collect variable" in {
    val variable = "hahahhhha"
    val path = s"root/${variable}/values".split("/").toSeq
    assert(path.matching(route2.path) == Some(variable | TNil))
  }

}