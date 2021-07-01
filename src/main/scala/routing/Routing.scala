package routing

enum PathSegment:
  case Segment(str: String)
  case Variable

import PathSegment._
import HttpMethod._

type HttpHandler = (HttpRequest, Seq[String]) => HttpResponse

case class PathWrapper(path: List[PathSegment])
case class Route(path: List[PathSegment], httpMethod: HttpMethod, requestHandler: HttpHandler)

extension (str: String)
  def /(str1: String) = PathWrapper(List(Segment(str), Segment(str1)))
  def /(segment: PathSegment) = PathWrapper(List(Segment(str), segment))

extension (wrapper: PathWrapper)
  def /(str: String) = PathWrapper(wrapper.path :+ Segment(str))
  def /(segment: PathSegment) = PathWrapper(wrapper.path :+ segment)
  def apply(values: (HttpMethod, HttpHandler)*) = values.map(value => Route(wrapper.path, value._1, value._2))

enum Node:
  case Pointer(map: Map[PathSegment, Node])
  case Handler(httpHandler: HttpHandler)

case class RouteTree(root: Node){
  def apply(path: String): HttpHandler = ???
}

object RouteTreeBuilder {
  def buildFrom(routes: Seq[Route]) = ???
}



