package server

import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString

import java.net.InetSocketAddress
import akka.actor._
import Tcp._
import json.JsonParsingException
import routing.ParseHttpRequest
import routing._


class HttpServer(address: String, port: Int, routes: Routes) extends Actor, ActorLogging {

  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress(address, port))

  var counter = 0

  def receive = {
    case b : Bound =>
    case CommandFailed(_: Bind) => context.stop(self)
    case Connected(remote, local) =>
      counter += 1
      val actorName = "http-request-handler" + counter
      log.debug(s"Established tcp connection with address: ${remote.toString}. Assigned actor: ${actorName} to manage the connection.")
      val handler = context.actorOf(Props[HttpRequestHandler](new HttpRequestHandler(routes)), actorName)
      val connection = sender()
      connection ! Register(handler)
  }
}


class HttpRequestHandler(routes: Routes) extends Actor, ActorLogging {
  import Tcp._
  def receive = {
    case Received(data) => handleErrors(data, handleData)
    case Tcp.Closed     => context.stop(self)
  }

  def errorMessage(message: String) = HttpResponse(
    HttpVersion.`Http/1.1`,
    StatusCode.BadRequest,
    Map(
      "Content-type" -> "application/json",
      "Error-message" -> message.replaceAll("\n", "")
    ),
    None
  )

  def sendResponse(response: HttpResponse) =
    val sender1 = sender()
    sender1 ! Tcp.Write(ByteString.fromString(response.httpString))
    sender1 ! Tcp.Close

  def handleErrors(data: ByteString, requestHandler: (ByteString) => HttpResponse) =
    try
      val response = requestHandler(data)
      sendResponse(response)
    catch
      case HttpRequestParsingException(msg) =>
        log.debug(s"Failed to parse http-request: \n${data.utf8String} \n\n Error message: ${msg}")
        sendResponse(errorMessage("Http-request is in invalid format."))
      case UnsupportedPath =>
        log.debug("Request path format is unsupported: Url")
        sendResponse(errorMessage("Path format is unsupported."))
      case MissingBodyException =>
        log.debug("Request is missing body.")
        sendResponse(errorMessage("Request is missing body."))
      case JsonParsingException(message) =>
        log.debug(s"Failed to parse json from body: ${message}")
        sendResponse(errorMessage("Invalid json-format"))
      case RouteResolvingException(path, method) =>
        log.debug(s"Given path does not exists: path: ${path}, http-method: ${method}")
        sendResponse(errorMessage("Invalid path: ${path}, http-method: ${method}."))
      case e: Exception =>
        log.warning(s"Failed to handle http-request: \n${data.utf8String} \n\n Error message: ${e.getMessage()}")
        sendResponse(errorMessage("Unkown error."))

  case object UnsupportedPath extends Exception
  case class RouteResolvingException(path: String, httpMethod: HttpMethod) extends Exception

  def handleData(data: ByteString): HttpResponse =
    val parsedMessage = ParseHttpRequest(data.utf8String)
    val path = resolvePath(parsedMessage.path)
    val resolvedRoutes = path.findRoute(routes)
    val resultRoute: Route[? <: Int] = resolvedRoutes.find(_.httpMethod == parsedMessage.httpMethod)
      .getOrElse(throw new RouteResolvingException(path.mkString("/"), parsedMessage.httpMethod))

    val matchingData = path.matching(resultRoute.path)
      .getOrElse(throw RouteResolvingException(path.mkString("/"), parsedMessage.httpMethod))

    resultRoute.handler((parsedMessage, matchingData))


  def resolvePath(path: HttpPath): Seq[String] = path match {
    case HttpPath.Route(value)  =>
      val splitted = value.trim.split("/").toSeq
      if splitted.head == "" then splitted.tail else splitted
    case HttpPath.Url(value)    => throw UnsupportedPath
  }
}

