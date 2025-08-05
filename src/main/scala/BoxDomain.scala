object BoxDomain {
  case class Point(row: Int, column: Int) {
    override def toString: String = s"(${row}, ${column})"
  }
  type Box = List[Point]
}

