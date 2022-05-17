package routing

import scala.compiletime.ops.int.S

trait RoutePath[A <: Int]
case object Root extends RoutePath[0]
case class Path[A <: Int](start: RoutePath[A], last: String) extends RoutePath[A]
case class VariablePath[A <: Int](list: RoutePath[A]) extends RoutePath[S[A]]

trait MatchPath[A <: Int]
case object Empty extends MatchPath[0]
case class MPath[A <: Int](head: String, tail: MatchPath[A]) extends MatchPath[A]
case class MVariablePath[A <: Int](tail: MatchPath[A]) extends MatchPath[S[A]]

trait VariableLike
case object Variable extends VariableLike

enum Params[A <: Int]:
  case TNil extends Params[0]
  case ParamList[A <: Int, T <: Params[A]](head: String, tail: T) extends Params[S[A]]

//TODO Routing should support query parameters

extension(str: String)
  def |[A <: Int, T <: Params[A]](tail: T) = Params.ParamList(str, tail)

extension[A <: Int](a: Params[A])
  def get(i: Int): String = a match {
    case TNil                  => throw new Exception("ParamList error: Index out of bound.")
    case ParamList(head, tail) => if i == 0 then head else tail.get(i-1)
  }

import Params._
type HttpHandler[A <: Int] = Tuple2[HttpRequest, Params[A]] => HttpResponse

case class Route[A <: Int](path: MatchPath[A], httpMethod: HttpMethod, handler: HttpHandler[A])

extension[A <: Int](path: MatchPath[A])
  def append(str: String): MatchPath[A] = path match {
    case Empty               => MPath(str, Empty)
    case MPath(head, tail)   => MPath(head, tail.append(str))
    case MVariablePath(list) => MVariablePath(list.append(str))
  }
  def appendVariable: MatchPath[S[A]] = path match {
    case Empty               => MVariablePath(Empty)
    case MPath(head, tail)   => MPath(head, tail.appendVariable)
    case MVariablePath(list) => MVariablePath(list.appendVariable)
  }

extension[A <: Int](path: RoutePath[A])
  def /(str: String) = new Path[A](path, str)
  def /(variable: VariableLike) = new VariablePath[A](path)
  def reverse: MatchPath[A] = path match {
    case Root               => Empty
    case Path(start, last)  => start.reverse.append(last)
    case VariablePath(list) => list.reverse.appendVariable
  }

object SimpleRouting {
  extension[A <: Int](path: RoutePath[A])
    def apply(tuple: (HttpMethod, HttpHandler[A])) = Seq(Route[A](path.reverse, tuple._1, tuple._2))

  extension[A <: Int](routes: Seq[Route[A]])
    def ~(tuple: (HttpMethod, HttpHandler[A])) = routes :+ Route[A](routes.head.path, tuple._1, tuple._2)
}



extension(pathList: Seq[String])
  def matching[A <: Int](path: MatchPath[A]): Option[Params[A]] =
    path match {
      case Empty if pathList.isEmpty  =>
        Some(TNil)
      case MPath(head, tail) if pathList.headOption == Some(head) =>
        pathList.tail.matching(tail)
      case MVariablePath(list) if pathList.nonEmpty =>
        val tailMatch = pathList.tail.matching(list)
        tailMatch.map(a => pathList.head | a)
      case _ => None
    }


