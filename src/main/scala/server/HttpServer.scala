package server

import akka.actor.{ Actor, ActorRef, Props }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress
import akka.actor._
import Tcp._


val simpleResponse =
  """|HTTP/1.1 200 OK
     |content-type:text/plain;charset=UTF-8
     |connection: keep-alive""".stripMargin


val simpleRequest =
  """
    |POST / HTTP/1.1
    |HOST: localhost:1080
    |content-type: text/plain;charset=UTF-8
    |content-length: 9
    |
    |dsfdsfsdf""".stripMargin

object Main extends App {
  given actorSystem: ActorSystem = ActorSystem()

  val serverRef = actorSystem.actorOf(Props[HttpServer](), "http-server")
}

class HttpServer extends Actor {

  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 1080))

  def receive = {
    case b : Bound =>
    case CommandFailed(_: Bind) => context.stop(self)
    case Connected(remote, local) =>
      val handler = context.actorOf(Props[HttpRequestHandler](), "http-request-handler" + System.currentTimeMillis())
      val connection = sender()
      connection ! Register(handler)
  }
}

class HttpRequestHandler extends Actor, ActorLogging {
  import Tcp._
  def receive = {
    case Received(data) =>
      //TODO message parsing
      //TODO logging
      val sender1 = sender()
      sender1 ! Tcp.Write(ByteString.fromString(simpleResponse))
      sender1 ! Tcp.Close
    case Tcp.Closed     => context.stop(self)
  }
}

