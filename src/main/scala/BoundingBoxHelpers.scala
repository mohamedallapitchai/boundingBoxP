import BoxDomain.*

import scala.math.abs


object BoundingBoxHelpers {
  
  def doesInputHaveSameColumnLength(list: List[String]): Boolean = {
    if (list.isEmpty) false
    else {
      val len = list.head.length
      list.forall(l => l.length == len)
    }
  }

 
  def doesInputHaveOnlyHyphenAndAsterisk(list: List[String]): Boolean = {
    list.forall(l => l.forall(c => c == '*' || c == '-'))
  }

  
  def canConnect(list: Box, currPoint: Point): Boolean = {
    val connectionFlag = list.exists(point => canEstablishConnection(point, currPoint))
    connectionFlag
  }

  
  def canEstablishConnection(otherPoint: Point, currPoint: Point): Boolean = {
    val row_other = otherPoint.row
    val col_other = otherPoint.column
    val curr_row = currPoint.row
    val curr_col = currPoint.column

    val row_diff = abs(row_other - curr_row)
    val col_diff = abs(col_other - curr_col)

    row_diff != col_diff && (row_diff == 1 && col_diff == 0) || (col_diff == 1 && row_diff == 0)
  }

  
  def addConnection(connectedSet: List[Box], point: Point, flag: Boolean = false): List[Box] = {
    connectedSet match {
      case Nil if !flag => List(point) :: connectedSet
      case Nil if flag => connectedSet
      case head :: tail => if (canConnect(head, point))
        (point :: head) :: addConnection(tail, point, true)
      else head :: addConnection(tail, point, flag)
    }
  }

  
  def connect(connectedSet: List[Box], line: String, row: Int): List[Box] = {
    (0 until line.length).foldLeft(connectedSet) {
      case (set, col) =>
        if (line(col) == '*') addConnection(set, Point(row + 1, col + 1)) else set
    }
  }

  
  def findArea(list: Box): (List[Point],Int) = {
    if (list.length <= 1) (Nil, 0)
    else {
      val groupByRow: Map[Int, List[Point]] = list.groupBy(pt => pt.row)
      val sortedKeys = groupByRow.keys.toList.sorted
      val maxRowKey = sortedKeys.last
      val minRowKey = sortedKeys.head
      //println(s"maxRowKey is ${maxRowKey}")
      //println(s"minRowKey is ${minRowKey}")
      val breadth = maxRowKey - minRowKey
      //println(s"breadth is ${breadth}")
      val maxRowMaxCol = groupByRow(maxRowKey).sorted((pt1, pt2) => pt1.column - pt2.column).last.column
      val minRowMinCol = groupByRow(minRowKey).sorted((pt1, pt2) => pt1.column - pt2.column).head.column
      
      val keyPoint = Point(minRowKey, minRowMinCol)
      // now create a list which excludes this point
      val exclusiveList = list.filter(p => p != keyPoint)
      
      // good now, for each point in exclusive list find the area between this point and the keyPoint
      // just retain the point which gives maximum area
      
      val listAreaTup = exclusiveList.foldLeft(List(keyPoint),0) {
        case (lat, pt) =>
          val b = pt.row - minRowKey
          val l = pt.column - minRowMinCol
          val area = b * l
          if (area >= lat._2)
            (pt::lat._1, area)
          else
            lat
      }
      listAreaTup
      
      /*val length = maxRowMaxCol - minRowMinCol
      //println(s"length is ${length}")
      // val groupByRowSorted = groupByRow.maxBy((key,_) => )
      // val breadth = groupByRow.  //._1 - groupByRow.min._1
      //val length = groupByRow.max._2 - groupByRow.min._2

      /* val lenPoints = list.sorted((pt1, pt2) => pt1.column - pt2.column)
       val length = lenPoints.last.column - lenPoints.head.column
       val breadthPoints = list.sorted((pt1, pt2) => pt1.row - pt2.row)
       val breadth = breadthPoints.last.row - breadthPoints.head.row*/
      val area = length * breadth
      //println(s"area is ${area}")
      area*/
    }
  }

  
  def excludeCurrentIndex(index: Int, list: List[Box]): List[Box] = {
    val (fh, sh) = list.splitAt(index + 1)
    fh.init ++ sh
  }

  
  def isUnique(givenBox: Box, otherBoxes: List[Box]): Boolean = {
    otherBoxes.forall(k => isNotOverlap(givenBox, k))
  }

  
  def isNotOverlap(currList: Box, otherList: Box): Boolean = {
    // finding left and right sides of the box (current box) we want to check for overlap
    val currListSortedByCol = currList.sorted((pt1, pt2) => pt1.column - pt2.column)
    val boxLeft = currListSortedByCol.head.column
    val boxRight = currListSortedByCol.last.column

    // finding top and bottom of the box (current box) we want to check for overlap
    val currListSortedByRow = currList.sorted((pt1, pt2) => pt1.row - pt2.row)
    val boxTop = currList.head.row
    val boxBottom = currList.last.row

    // finding the left and right sides of other box we want to compare with the current one
    val otherListSortedByCol = otherList.sorted((pt1, pt2) => pt1.column - pt2.column)
    val otherBoxLeft = otherListSortedByCol.head.column
    val otherBoxRight = otherListSortedByCol.last.column

    // finding the top and bottom sides of other box we want to compare with the current one
    val otherListSortedByRow = otherList.sorted((pt1, pt2) => pt1.row - pt2.row)
    val otherBoxTop = otherListSortedByRow.head.row
    val otherBoxBottom = otherListSortedByRow.last.row

    checkNotOverlap(boxLeft, boxRight, boxTop, boxBottom, otherBoxLeft, otherBoxRight, otherBoxTop, otherBoxBottom)
  }

  
  def checkNotOverlap(boxLeft: Int, boxRight: Int, boxTop: Int,
                      boxBottom: Int, otherBoxLeft: Int, otherBoxRight: Int, otherBoxTop: Int,
                      otherBoxBottom: Int): Boolean = {
    /*println(s"${boxTop}, ${boxLeft}, ${boxBottom}, ${boxRight}, and other box: ${otherBoxTop}, ${otherBoxLeft}," +
      s"${otherBoxBottom}, ${otherBoxRight}")*/
    boxRight <= otherBoxLeft || otherBoxRight <= boxLeft || boxBottom <= otherBoxTop || boxTop >= otherBoxBottom

  }

  
  object BoxSorter extends Ordering[Point] {
    override def compare(pt1: Point, pt2: Point): Int = {
      if (pt1.row == pt2.row) pt1.column - pt2.column
      else pt1.row - pt2.row
    }
  }

}
