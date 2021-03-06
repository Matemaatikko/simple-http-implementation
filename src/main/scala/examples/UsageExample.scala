package examples

import akka.actor.{ActorSystem, Props}
import server.HttpServer

object UsageExample extends App{

  //Data
  case class User(userId: String, name: String, age: Int)

  //Json conversion
  import json.conversion._
  import JsonConversions._
  import JsonConversions.given

  given Class[User] = classOf[User]
  given JsonConversion[User] = jsonConversion3(User.apply)

  //Actions

  import routing.StatusCode._

  def getUser(userId: String) =
    println("Sending user data: " + userId)
    OK -> User(userId, "Joe", 99)

  def ping = () =>
    println("PING")
    OK -> ()

  def updateUser(user: User, userId: String) =
    println("Updating user data: " + user.toString)
    OK -> ()

  //Routing

  import routing._
  import HttpMethod._
  import RoutingUtils._

  val routes: Seq[Route[? <: Int]] =
    (Root / "users" / Variable)
      (GET  -> getUser.convert) ~
      (POST -> updateUser.convert)
    ++
      (Root / "ping")
        (GET  -> ping.convert)

  val routeTree = RouteTreeBuilder.build(routes)

  // Server
  given actorSystem: ActorSystem = ActorSystem()

  class Server extends HttpServer("localhost", 8080, routeTree)
  val serverRef = actorSystem.actorOf(Props[Server](), "http-server")

}
