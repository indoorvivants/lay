package lay

enum Param:
  case Padding(left: Int, right: Int, top: Int, bottom: Int)
  case ChildGap(w: Int)
  case Width(w: Int)
  case Height(h: Int)
  case FitWidth, FitHeight
end Param

import Param.*
export Param.*

type BoxParam = Padding | Width | Height | FitWidth.type | FitHeight.type
type ColumnParam = BoxParam | ChildGap
type TextParam = BoxParam
type RowParam = BoxParam | ChildGap

enum NodeType(val params: List[Param]):
  case Column private[lay] (_params: List[Param]) extends NodeType(_params)
  case Row private[lay] (_params: List[Param]) extends NodeType(_params)
  case Text private[lay] (_params: List[Param]) extends NodeType(_params)

end NodeType

inline def getParams[T >: Param](n: List[T]) =
  n.collect { case param: Param => param }

inline def getChildren[T >: Node](l: List[T]) =
  l.collect { case node: Node => node }

def Column(nest: (ColumnParam | Node)*) =
  val params = getParams(nest.toList)
  val base = Node(
    topo = 0,
    NodeType.Column(params),
    Nil
  )
  val children = getChildren(nest.toList).zipWithIndex.map((n, i) =>
    n.copy(
      topo = i
    )
  )

  base.copy(
    children = children
    // topo = children.maxByOption(_.topo).map(_.topo + 1).getOrElse(1)
  )
end Column

def Row(nest: (RowParam | Node)*) =
  val params = getParams(nest.toList)
  val base = Node(
    topo = 0,
    NodeType.Row(params),
    // None,
    Nil
  )

  val children = getChildren(nest.toList).zipWithIndex.map((n, i) =>
    n.copy(
      // parent = Some(base),
      topo = i + 1
    )
  )

  base.copy(
    children = children
  )
end Row

def Text(params: TextParam*) =
  Node(
    topo = 0,
    NodeType.Text(params.toList),
    // None,
    Nil
  )

case class Node private[lay] (
    topo: Int,
    tpe: NodeType,
    children: List[Node] = Nil
)
end Node

def number(node: Node): Node =
  var counter = 0
  def go(node: Node): Node =
    val id = counter
    counter += 1
    node.copy(topo = id, children = node.children.map(go))
  go(node)
end number

extension (p: List[Param])
  def width = p.collectFirst { case Width(v) => v }
  def height = p.collectFirst { case Height(v) => v }
  def padding: Padding =
    p.collectFirst { case v: Padding => v }.getOrElse(Padding(0, 0, 0, 0))
  def childGap = p.collectFirst { case ChildGap(v) => v }.getOrElse(0)
end extension

case class Size(width: Int, height: Int)
def fitSizing(root: Node): Map[Node, Size] =
  def go(n: List[Node], currentSizes: Map[Node, Size]): Map[Node, Size] =
    n match
      case Nil          => currentSizes
      case head :: tail =>
        if head.children.isEmpty then
          val width = head.tpe.params.width.getOrElse(0)
          val height = head.tpe.params.height.getOrElse(0)
          go(tail, currentSizes) + (head -> Size(width, height))
        else
          val childrenSizing = go(head.children, currentSizes)
          val params = head.tpe.params
          val padding: Param.Padding =
            params.padding
          val childGap = params.childGap
          val sizing = head.children.map(childrenSizing.apply(_))

          val (totalWidth, totalHeight) =
            head.tpe match
              case _: NodeType.Row =>
                (
                  sizing
                    .map(_.width)
                    .sum + padding.left + padding.right + childGap * (sizing.length - 1),
                  sizing.map(_.height).max + padding.top + padding.bottom
                )
              case _: NodeType.Column =>
                (
                  sizing.map(_.width).max + padding.left + padding.right,
                  sizing
                    .map(_.height)
                    .sum + padding.top + padding.bottom + childGap * (sizing.length - 1)
                )
            end match

          end val

          childrenSizing ++ go(tail, currentSizes) +
            (head -> Size(
              totalWidth,
              totalHeight
            ))
        end if
    end match
  end go

  go(List(root), Map.empty)

end fitSizing

case class Position(x: Int, y: Int)
def positioning(root: Node, sizing: Map[Node, Size]): Map[Node, Position] =
  def draw(
      n: Node,
      position: Position
  ): Map[Node, Position] =
    val Position(startX, startY) = position
    n.tpe match
      case lay.NodeType.Column(_params) =>
        val posX = startX + n.tpe.params.padding.left
        var posY = startY + n.tpe.params.padding.top
        val childGap = n.tpe.params.childGap
        Map(n -> position) ++ n.children
          .flatMap: child =>
            val newStuff = draw(child, Position(posX, posY))
            posY += sizing(child).height + childGap
            newStuff
          .toMap
      case lay.NodeType.Row(_params) =>
        var posX = startX + n.tpe.params.padding.left
        val posY = startY + n.tpe.params.padding.top
        val childGap = n.tpe.params.childGap

        Map(n -> position) ++ n.children
          .flatMap: child =>
            val newStuff = draw(child, Position(posX, posY))
            posX += sizing(child).width + childGap
            newStuff
          .toMap

      case lay.NodeType.Text(_params) =>
        Map(n -> position)
    end match
  end draw

  draw(root, Position(0, 0))
end positioning
