import BoundingBoxHelpers.*
import BoxDomain.*
import BoundingBox._
import org.scalatest.funsuite.AnyFunSuite

class BoundingBoxTest_FindingNonOverlappingBox extends AnyFunSuite {
  // adjacent points in x axis can establish connection
  test("Given a list of points (boxes) " +
    "List(Point(1,2),Point(2,3)), List(Point(3,4), Point(5,6)) " +
    "and List(Point(7,8), Point(8,9)) and an index 1 " +
    "calling excludeCurrentIndex should return " +
    "the boxes except the box corresponding to index.") {
    val listOfBoxes: List[Box] = List(
      List(Point(1, 2), Point(2, 3)),
      List(Point(3, 4), Point(5, 6)),
      List(Point(7, 8), Point(8, 9))
    )

    val index = 1
    val remainingBoxes: List[Box] = excludeCurrentIndex(index, listOfBoxes)
    assert(remainingBoxes.length == 2)
    assert(
      remainingBoxes == List(List(Point(1, 2), Point(2, 3)), List(Point(7, 8), Point(8, 9)))
    )
  }

  test("Given a list of points (boxes) " +
    "List(Point(1,2),Point(2,3)), List(Point(3,4), Point(5,6)) " +
    "and List(Point(7,8), Point(8,9)) and an index 2 " +
    "calling excludeCurrentIndex should return " +
    "the boxes except the box corresponding to index.") {
    val listOfBoxes: List[Box] = List(
      List(Point(1, 2), Point(2, 3)),
      List(Point(3, 4), Point(5, 6)),
      List(Point(7, 8), Point(8, 9))
    )

    val index = 2
    val remainingBoxes: List[Box] = excludeCurrentIndex(index, listOfBoxes)
    assert(remainingBoxes.length == 2)
    assert(
      remainingBoxes  == List(List(Point(1, 2), Point(2, 3)), List(Point(3, 4), Point(5, 6)))
    )
  }


  test("When the points left=1, right=2, top=2 and bottom=3 of box1" +
    "and the points left=3, right=5, top=4 and bottom=6 given, the method " +
    "checkNotOverlap gives true as both boxes does not overlap") {
    val boxLeft = 1
    val boxRight = 2
    val boxTop = 2
    val boxBottom = 3

    val otherBoxLeft = 3
    val otherBoxRight = 5
    val otherBoxTop = 4
    val otherBoxBottom = 6

    val isNotOverlap: Boolean = checkNotOverlap(boxLeft, boxRight, boxTop, boxBottom, otherBoxLeft, otherBoxRight, otherBoxTop,
      otherBoxBottom)
    assert(isNotOverlap)
  }

  test("When the points left=10, right=12, top=1 and bottom=3 of box1" +
    "and the points left=9, right=12, top=2 and bottom=4 given, the method " +
    "checkNotOverlap gives false as both boxes does overlap") {
    val boxLeft = 10
    val boxRight = 12
    val boxTop = 1
    val boxBottom = 3

    val otherBoxLeft = 9
    val otherBoxRight = 12
    val otherBoxTop = 2
    val otherBoxBottom = 4

    val isNotOverlap: Boolean = checkNotOverlap(boxLeft, boxRight, boxTop, boxBottom, otherBoxLeft, otherBoxRight, otherBoxTop,
      otherBoxBottom)
    assert(!isNotOverlap)
  }


  test("Given box1 List(Point(1, 1), Point(1, 2), Point(2, 2)) and " +
    "   box2  List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), " +
    "        Point(2, 11), Pont(3, 11), Point(3, 12))" +
    "isNotOverlap method returns true as these two boxes don't overlap") {
    val box1: Box = List(Point(1, 1), Point(1, 2), Point(2, 2))
    val box2: Box  = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val isNotOverlapping: Boolean = isNotOverlap(box1, box2)
    assert(isNotOverlapping)
  }

  test("Given box1 List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), " +
    "         Point(3, 8), Point(4, 8), Point(4, 9), Point(4, 10)) and " +
    "   box2  List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), " +
    "        Point(2, 11), Pont(3, 11), Point(3, 12))" +
    "isNotOverlap method returns true as these two boxes don't overlap") {
    val box1: Box = List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8),
      Point(4, 9), Point(4, 10))
    val box2: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val isNotOverlapping: Boolean = isNotOverlap(box1, box2)
    assert(isNotOverlapping)
  }

  test("Given box1 List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12)) and " +
    "   box2  List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), " +
    "        Point(2, 11), Pont(3, 11), Point(3, 12))" +
    "isNotOverlap method returns true as these two boxes overlap") {
    val box1: Box = List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val box2: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val isNotOverlapping: Boolean = isNotOverlap(box1, box2)
    assert(!isNotOverlapping)
  }


  test("Given box1 List(Point(1, 1), Point(1, 2)) and " +
    "   box2  List(Point(1, 4), Point(1, 5), Point(1, 6), Point(1, 7), " +
    "isNotOverlap method returns true as these two boxes don't overlap") {
    val box1: Box = List(Point(1, 1), Point(1, 2))
    val box2: Box = List(Point(1, 4), Point(1, 5), Point(1, 6), Point(1, 7))
    val isNotOverlapping: Boolean = isNotOverlap(box1, box2)
    assert(isNotOverlapping)
  }

  test("Given box1 List(Point(1, 1), Point(1, 2)) and " +
    "   box2  List(Point(2, 3), Point(2, 4), Point(2, 5), Point(2, 6), " +
    "isNotOverlap method returns true as these two boxes don't overlap") {
    val box1: Box = List(Point(1, 1), Point(1, 2))
    val box2: Box = List(Point(2, 3), Point(2, 4), Point(2, 5), Point(2, 6))
    val isNotOverlapping: Boolean = isNotOverlap(box1, box2)
    assert(isNotOverlapping)
  }

  test("Given box1 List(Point(1, 1), Point(1, 2)) and " +
    "   list of boxes" +
    "   box2 List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))" +
    "   box3 List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8)," +
    "             Point(4, 9), Point(4, 10))" +
    "   box4 List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))" +
    "   the method isUnique determines box1 is unique (don't overlap) when compare to other boxes. "
  ) {
    val box1: Box = List(Point(1, 1), Point(1, 2))
    val box2: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val box3: Box = List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8),
      Point(4, 9), Point(4, 10))
    val box4: Box = List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val listOfBoxes: List[Box] = List(box2, box3, box4)
    val isUniqueBox: Boolean = isUnique(box1, listOfBoxes)
    assert(isUniqueBox)
  }

  test("Given " +
    "   box1 List(Point(1, 1), Point(1, 2)) " +
    "   and list of boxes" +
    "   box2 List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11))" +
    "   box3 List(Point(2, 5), Point(2, 6))" +
    "   box4 List(Point(2, 9), Point(2, 10), Point(2, 11))" +
    "   the method findUniqueSet() method should return all boxes as they don't overlap. "
  ) {
    val box1: Box = List(Point(1, 1), Point(1, 2))
    val box2: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11))
    val box3: Box = List(Point(2, 5), Point(2, 6))
    val box4: Box = List(Point(2, 9), Point(2, 10), Point(2, 11))
    val listOfBoxes: List[Box] = List(box1, box2, box3, box4)
    val uniqueBoxes: List[Box] = findUniqueSet(listOfBoxes)
    assert(uniqueBoxes == List(box1,box2,box3,box4))
  }
  
}

