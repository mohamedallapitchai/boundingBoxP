import BoundingBoxHelpers.*
import BoxDomain.*

import java.io.File
import scala.collection.immutable.List
import scala.io.Source
import scala.io.StdIn.readLine



object BoundingBox {


  def buildConnectedBoxes(list: List[String]): List[Box] = {
    list.indices.foldLeft(Nil: List[Box]) {
      case (connectedSet, rowNum) => connect(connectedSet, list(rowNum), rowNum)
    }.map(l => l.sorted(BoxSorter))
  }


  def findUniqueSet(list: List[Box]): List[Box] = {
    val result = for {
      f <- list.indices
      if isUnique(list(f), excludeCurrentIndex(f, list))
    } yield (list(f))
    result.toList
  }


  def findMaxAreaBoxCoordinates(list: List[Box]): Option[List[Box]] = {
    //println(s"finxMaxAreaBoxCoordinates ${list}")
    val pointAndArea = list.foldLeft(Nil: List[Box], 0) {
      case (zeroArea, sublist) =>
        val area = findArea(sublist)
        if (area > zeroArea._2)
          (List(sublist), area)
        else if (area == zeroArea._2) (sublist :: zeroArea._1, area)
        else zeroArea
    }
    if (pointAndArea._2 == 0) {
      val singleDetached = pointAndArea._1.filter(box => box.length == 1)
      val generatedPair: List[Box] = singleDetached.map(box => List(box.head, box.head))
      Some(generatedPair)
    }
    else {
      val sortedPoints: List[Box] = pointAndArea._1.map(bx => bx.sorted((pt1, pt2) => pt1.row - pt2.row))
      val onlyMinAndMax: List[Box] = sortedPoints.map(bx => List(bx.head, bx.last))
      Some(onlyMinAndMax)
    }
  }

  def open(path: String) = new File(path)

  extension (f: File) {
    def read() = Source.fromFile(f).getLines()
  }


  def main(args: Array[String]) = {
    //val linesOfInput = Iterator.continually(readLine()).takeWhile(_.nonEmpty).toList
    val listOfLines = open("resources/groups.txt").read().toList
    //val lines = scala.io.Source.stdin.getLines()
    //val linesOfInput = lines.toList
    //println(lines)
    // println(linesOfInput)

    assert(
      doesInputHaveSameColumnLength(listOfLines),
      "Error: the column length of each input line is not same or empty input"
    )

    assert(
      doesInputHaveOnlyHyphenAndAsterisk(listOfLines),
      "Error: the characters should only contain either '*' or '-'"
    )

    val output = buildConnectedBoxes(listOfLines)
    val connectedNonOverlappingSet = findUniqueSet(output)
    val maxAreaPointsOpt = findMaxAreaBoxCoordinates(connectedNonOverlappingSet)
    if (maxAreaPointsOpt.get.isEmpty) println(s"()")
    else {
      val results: List[Box] = maxAreaPointsOpt.get
      val finalResultStr = results.map(bx => s"${bx.head}${bx.last}").mkString(",")
      print(s"${finalResultStr}")
    }
  }
}
