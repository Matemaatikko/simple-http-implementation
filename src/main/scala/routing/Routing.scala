package routing

import scala.compiletime.ops.int.S

enum RoutePath[A <: Int]:
  case Root extends RoutePath[0]
  case Path[A <: Int](start: RoutePath[A], last: String) extends RoutePath[A]
  case VariablePath[A <: Int](list: RoutePath[A]) extends RoutePath[S[A]]

enum MatchPath[A <: Int]:
  case Empty extends MatchPath[0]
  case MPath[A <: Int](head: String, tail: MatchPath[A]) extends MatchPath[A]
  case MVariablePath[A <: Int](tail: MatchPath[A]) extends MatchPath[S[A]]

enum VariableLike:
  case Variable

enum Params[A <: Int]:
  case TNil extends Params[0]
  case ParamList[A <: Int, T <: Params[A]](head: String, tail: T) extends Params[S[A]]

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

import RoutePath._
import MatchPath._

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
  def apply(values: (HttpMethod, HttpHandler[A])*) = values.map(value => Route[A](path.reverse, value._1, value._2))

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

//TODO Add Json-convert and login boilerplate


