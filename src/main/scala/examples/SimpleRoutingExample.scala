package examples

import akka.actor.{ActorSystem, Props}
import examples.UsageExample.User
import json.{JsObject, JsValue, JsonPrinter}
import routing._
import server.HttpServer


object SimpleRoutingExample extends App {

  //Data
  case class User(userId: String, name: String, age: Int)

  //Json conversion
  import json.conversion._
  import JsonConversions._
  import JsonConversions.given

  given Class[User] = classOf[User]
  val conversion = jsonConversion3(User.apply)

  //Actions

  import routing.StatusCode._

  def getUser(request: HttpRequest, userId: String): HttpResponse =
    println("Sending user data: " + userId)

    val user = User(userId, "Joe", 99)
    val bodyJs: JsValue = conversion.toJson(user)
    val bodyString = JsonPrinter.print(bodyJs)

    HttpResponse(
        routing.HttpVersion.`Http/1.0`, OK,
        Map(
          "Content-type"->"application/json",
          "Content-length" -> bodyString.length.toString
        ),
        Some(bodyString)
      )

  def getUser2: HttpHandler[1] = args => getUser(args._1, args._2.get(0))

  //Routing

  import routing._
  import HttpMethod._
  import SimpleRouting._

  val routes: Seq[Route[? <: Int]] =
    (Root / "users" / Variable)
      (GET  -> getUser2)

  val routeTree = RouteTreeBuilder.build(routes)

  // Server
  given actorSystem: ActorSystem = ActorSystem()

  class Server extends HttpServer("localhost", 8080, routeTree)
  val serverRef = actorSystem.actorOf(Props[Server](), "http-server")

}
