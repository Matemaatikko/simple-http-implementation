package routing

import org.scalatest._
import flatspec._
import json.conversion.JsonConversion
import matchers._

class RoutingUtilsTest extends AnyFlatSpec with should.Matchers {

  import RoutingUtils._
  import HttpMethod._

  def fun(a: Seq[String], arg: String) = StatusCode.OK -> a.head
  def fun1(a: Seq[String], arg: String) = StatusCode.OK -> ()

  import json.conversion.JsonConversions.given

  val routes: Seq[Route[? <: Int]] =
    (Root / "root" / Variable / "values")
      (GET  -> fun.convert) ~
      (POST -> fun1.convert)

  val routeString = "/root/123/values"
  val request = HttpRequest(HttpMethod.POST, HttpPath.Route(routeString), HttpVersion.`Http/1.1`, Nil, Some("[\"head\"]"))

  import routing.Params.TNil

  "RoutingUtils" should "work" in {
    val route: Route[? <: Int] = routes.head
    val params = (routeString.split("/").toSeq.tail).matching(route.path).get
    val response = route.handler((request, params))
    assert(response.body == Some("\"head\""))
  }
}