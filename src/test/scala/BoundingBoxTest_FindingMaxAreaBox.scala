import BoundingBoxHelpers._
import BoundingBox._
import BoxDomain._
import org.scalatest.funsuite.AnyFunSuite

class BoundingBoxTest_FindingMaxAreaBox extends AnyFunSuite {

  test("Given a list of boxes " +
    "Box1: List(Point(1, 1), Point(1, 2), Point(2, 2))," +
    "Box2: List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), " +
    "Point(3, 12))," +
    "Box3: List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8), " +
    "Point(4, 9), Point(4, 10))," +
    "Box4:  List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12)) and " +
    "calling the function findUniqueSet would give the Box List(Point(1, 1), Point(1, 2), Point(2, 2))" +
    "as box1 does not overlap with other boxes") {
    val box1: Box = List(Point(1, 1), Point(1, 2), Point(2, 2))
    val box2: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val box3: Box = List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8),
      Point(4, 9), Point(4, 10))
    val box4: Box = List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val listOfBoxes: List[Box] = List(box1, box2, box3, box4)

    val uniqueBoxes: List[Box] = findUniqueSet(listOfBoxes)
    assert(uniqueBoxes == List(box1))
  }

  test("Given a list of boxes " +
    "Box1: List(Point(1, 1), Point(1, 2), Point(2, 2))," +
    "Box2: List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), " +
    "Point(3, 11), Point(3, 12))," +
    "Box3: List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), " +
    "Point(3, 8), Point(4, 8), Point(4, 9), Point(4, 10)) and " +
    "calling the function findUniqueSet would give all boxes as they don't overlap each other") {
    val box1: Box = List(Point(1, 1), Point(1, 2), Point(2, 2))
    val box2: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val box3: Box = List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8),
      Point(4, 9), Point(4, 10))
    val listOfBoxes: List[Box] = List(box1, box2, box3)

    val uniqueBoxes: List[Box] = findUniqueSet(listOfBoxes)
    assert(uniqueBoxes.length == 3)
    assert(uniqueBoxes == listOfBoxes)
  }

  // finding area
  test("Given a  boxes " +
    "Box: List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), " +
    "Point(3, 8), Point(4, 8), Point(4, 9), Point(4, 10)) and " +
    "calling the function findArea() would give the area 10") {
    val box: Box = List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8),
      Point(4, 9), Point(4, 10))

    val area: Int = findArea(box)
    assert(area == 10)
  }

  test("Given an empty boxes " +
    "calling the findArea() should return 0 ") {
    val box: Box = Nil

    val area: Int = findArea(box)
    assert(area == 0)
  }

  test("Given a  box " +
    "Box: List(Point(1, 1), Point(1, 2), Point(1, 3)" +
    "calling the function findArea() would give the area 0 as this box does not form a rectangle") {
    val box: Box = List(Point(1, 1), Point(1, 2), Point(1, 3))

    val area: Int = findArea(box)
    assert(area == 0)
  }

  test("Given a  box " +
    "Box: List(Point(1, 1), Point(1, 2), Point(2, 2)" +
    "calling the function findArea() would give the area 4 as this box does not form a rectangle") {
    val box: Box = List(Point(1, 1), Point(1, 2), Point(2, 2))

    val area: Int = findArea(box)
    assert(area == 1)
  }

  test("Given a  box " +
    "Box: List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11))" +
    "calling the function findArea() should give the area 1 ") {
    val box: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11))

    val area: Int = findArea(box)
    assert(area == 1)
  }

  List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11))


  test("Given a list of boxes " +
    "Box1: List(Point(1, 1), Point(1, 2), Point(2, 2))," +
    "Box2: List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), " +
    "Point(3, 12))," +
    "Box3: List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8), " +
    "Point(4, 9), Point(4, 10))," +
    "Box4:  List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12)) and " +
    "calling findMaxAreaBox() method would find the maximum area box and return the " +
    "top left and bottom right coordinates which is (2,5) (4,10) for box3 and the area is 10") {

    val box1: Box = List(Point(1, 1), Point(1, 2), Point(2, 2))
    val box2: Box = List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val box3: Box = List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8),
      Point(4, 9), Point(4, 10))
    val box4: Box = List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    val listOfBoxes = List(box1, box2, box3, box4)

    val maxAreaCooridnates: Option[List[Box]] = findMaxAreaBoxCoordinates(listOfBoxes)
    val box = List(Point(2, 5), Point(4, 10))
    assert(maxAreaCooridnates.contains(List(box)))

  }
}