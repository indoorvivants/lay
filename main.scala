//> using scala 3.8.2-RC3
//> using dep com.lihaoyi::pprint::0.9.6
import lay.*

@main def hello =
  val node =
    number(
      Column(
        Padding(3, 3, 3, 3),
        ChildGap(2),
        Row(
          ChildGap(3),
          Padding(2, 2, 2, 2),
          Box(Width(30), Height(5)),
          Box(Width(30), Height(5)),
          Box(Width(11), Height(5))
        ),
        Row(
          Padding(4, 4, 4, 4),
          Box(Width(37), Height(10))
        )
      )
    )
  end node

  pprint.pprintln(number(node))

  val sizes = fitSizing(number(node))
  val positions = positioning(number(node), sizes)

  val canvas =
    Array.tabulate(sizes(node).height, sizes(node).width)((x, y) => " ")

  def draw(n: Node): Unit =
    def box(node: Node, f: (Int, Int) => Unit) =
      val p = positions(node)
      val sz = sizes(node)
      for
        x <- p.x until p.x + sz.width
        y <- p.y until p.y + sz.height
      do f(x, y)
    end box

    n.tpe match
      case lay.NodeType.Column(_params) =>
        box(n, (x, y) => canvas(y)(x) = fansi.Color.Blue("C").render)
        n.children.foreach(draw(_))

      case lay.NodeType.Row(_params) =>
        box(n, (x, y) => canvas(y)(x) = fansi.Color.Green("R").render)
        n.children.foreach(draw(_))

      case lay.NodeType.Text(_params) =>
        box(n, (x, y) => canvas(y)(x) = fansi.Color.Red("T").render)
    end match
  end draw

  draw(node)

  // def draw(n: Node, startX: Int, startY: Int): Unit =
  //   n.tpe match
  //     case lay.NodeType.Column(_params) =>
  //       for
  //         x <- startX until startX + sizing(n).width
  //         y <- startY until startY + sizing(n).height
  //       do canvas(y)(x) = fansi.Color.Blue("C").render
  //       val posX = startX + n.tpe.params.padding.left
  //       var posY = startY + n.tpe.params.padding.top
  //       val childGap = n.tpe.params.childGap
  //       n.children.foreach: child =>
  //         draw(child, posX, posY)
  //         posY += sizing(child).height + childGap
  //     case lay.NodeType.Row(_params) =>
  //       for
  //         x <- startX until startX + sizing(n).width
  //         y <- startY until startY + sizing(n).height
  //       do canvas(y)(x) = fansi.Color.Green("R").render

  //       var posX = startX + n.tpe.params.padding.left
  //       val posY = startY + n.tpe.params.padding.top
  //       val childGap = n.tpe.params.childGap

  //       n.children.foreach: child =>
  //         draw(child, posX, posY)
  //         posX += sizing(child).width + childGap

  //     case lay.NodeType.Text(_params) =>
  //       for
  //         x <- startX until startX + sizing(n).width
  //         y <- startY until startY + sizing(n).height
  //       do canvas(y)(x) = fansi.Color.Red("T").render

  // draw(node, 0, 0)

  canvas.map(_.mkString).foreach(println)
end hello
