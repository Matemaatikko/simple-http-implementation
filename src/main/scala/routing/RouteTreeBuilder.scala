package routing


enum Segment:
  case Value(str: String)
  case Variable

enum TreeElem:
  case Leaf[A <: Int](route: Route[A])
  case Branch(branches: Map[Segment, Branch], leafs: Seq[Leaf[? <: Int]])

type Routes = TreeElem.Branch

val emptyBranch: Branch = Branch(Map(), Nil)

import TreeElem._
import MatchPath._
import Segment._
import routing.Params.TNil

extension(root: Branch)
  def add[A <: Int](route: Route[A]): Branch =
    addImpl(route.path, route)

  def addImpl[A <: Int, B <: Int](path: MatchPath[A], route: Route[B]): Branch =
    path match {
      case Empty =>
        root.copy(leafs = root.leafs :+ Leaf(route))
      case MPath(head, tail) if root.branches.keys.toSeq.contains(Value(head)) =>
        root.copy(branches =
          root.branches.map((key, branch) =>  if key == Value(head) then key -> branch.addImpl(tail, route) else key -> branch))
      case MPath(head, tail) if !root.branches.keys.toSeq.contains(Value(head))   =>
        root.copy(branches =
          root.branches + (Value(head) -> emptyBranch.addImpl(tail, route)))
      case MVariablePath(tail) if root.branches.keys.toSeq.contains(Variable) =>
        root.copy(branches =
          root.branches.map((key, branch) => if key == Variable then key -> branch.addImpl(tail, route) else key -> branch))
      case MVariablePath(tail) if !root.branches.keys.toSeq.contains(Variable) =>
        root.copy(branches =
          root.branches + (Variable -> emptyBranch.addImpl(tail, route)))
    }

object RouteTreeBuilder {
  def build(routes: Seq[Route[? <: Int]]): Branch =
    routes.foldLeft(emptyBranch)( (branch: Branch, route: Route[? <: Int]) => branch.add(route))
}

extension(pathList: Seq[String])
  def findRoute(root: Branch): Seq[Route[? <: Int]] =
    if pathList.isEmpty then root.leafs.map(_.route)
    else
      val branchOpt = root.branches.get(Value(pathList.head)).orElse(root.branches.get(Variable))
      branchOpt.map(pathList.tail.findRoute(_)).getOrElse(Nil)

