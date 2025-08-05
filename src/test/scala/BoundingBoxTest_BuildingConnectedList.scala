import BoundingBox.*
import BoundingBoxHelpers.*
import BoxDomain.*
import org.scalatest.funsuite.AnyFunSuite

class BoundingBoxTest_BuildingConnectedList extends AnyFunSuite {
  // adjacent points in x axis can establish connection
  test("Given a point (1,1) and a point (1,2), " +
    "the canEstablishConnection should return true ") {
    val point1 = Point(1, 1)
    val point2 = Point(1, 2)
    assert(canEstablishConnection(point1, point2))
  }

  // adjacent points in y axis can establish connection
  test("Given a point (1,1) and a point (2, 1), " +
    "the canEstablishConnection should return true ") {
    val point1 = Point(1, 1)
    val point2 = Point(2, 1)
    assert(canEstablishConnection(point1, point2))
  }

  // adjacent points in y axis can establish connection
  test("Given a point (1,2) and a point (2, 2), " +
    "the canEstablishConnection should return true ") {
    val point1 = Point(1, 2)
    val point2 = Point(2, 2)
    assert(canEstablishConnection(point1, point2))
  }

  // adjacent points in x axis can establish connect
  test("Given a point (2,1) and a point (2, 2), " +
    "the canEstablishConnection should return true ") {
    val point1 = Point(2, 1)
    val point2 = Point(2, 2)
    assert(canEstablishConnection(point1, point2))
  }

  test("Given a point (2,1) and a point (1, 1), " +
    "the canEstablishConnection should return true ") {
    val point1 = Point(2, 1)
    val point2 = Point(1, 1)
    assert(canEstablishConnection(point1, point2))
  }

  // diagonal points could not establish connection
  test("Given a point (1,1) and a point (2, 2), " +
    "the canEstablishConnection should return false ") {
    val point1 = Point(1, 1)
    val point2 = Point(2, 2)
    assert(!canEstablishConnection(point1, point2))
  }

  // same points could not establish connection
  test("Given a point (1,1) and a point (1, 1), " +
    "the canEstablishConnection should return false ") {
    val point1 = Point(1, 1)
    val point2 = Point(1, 1)
    assert(!canEstablishConnection(point1, point2))
  }

  // points not adjacent could not establish connection
  test("Given a point (1,1) and a point (1, 10), " +
    "the canEstablishConnection should return false ") {
    val point1 = Point(1, 1)
    val point2 = Point(1, 10)
    assert(!canEstablishConnection(point1, point2))
  }

  test("Given a list of points (2, 5), (2, 6), (3, 6)" +
    "and  a given point (3,7) the canConnect () method should return true") {

    val pt1 = Point(2, 5)
    val pt2 = Point(2, 6)
    val pt3 = Point(3, 6)
    val listOfPoints = List(pt1, pt2, pt3)
    val currPoint = Point(3, 7)
    assert(canConnect(listOfPoints, currPoint))
  }

  test("Given a empty list of points)" +
    "and  a given point (3,7) the canConnect () method should return false") {
    val currPoint = Point(3, 7)
    assert(!canConnect(List(), currPoint))
  }

  test("Given a list of points (2, 5), (2, 6), (3, 6)" +
    "and  a given point (4,12) the canConnect () method should return false") {

    val pt1 = Point(2, 5)
    val pt2 = Point(2, 6)
    val pt3 = Point(3, 6)
    val listOfPoints = List(pt1, pt2, pt3)
    val currPoint = Point(4, 12)
    assert(!canConnect(listOfPoints, currPoint))
  }

  //add connection test
  test("Given a list of Boxes " +
    " List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 11), Point(3, 11), Point(3, 12))" +
    "and List(Point(2, 9), (2, 11), (3, 11), (3, 12))" +
    "and a given point Point(2,10), calling " +
    "addConnection() should add Point(2,10) to both the lists because" +
    "from both the lists, the point Point(2,10) can be connected") {

    val listsOfBoxes: List[Box] = List(
      List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 11), Point(3, 11), Point(3, 12)),
      List(Point(2, 9), Point(2, 11), Point(3, 11), Point(3, 12))
    )
    val givenPoint = Point(2, 10)

    val connectedLists: List[Box] = addConnection(listsOfBoxes, givenPoint)
    assert(connectedLists.size == 2)
    assert(connectedLists.head.length == 7)
    assert(connectedLists.last.length == 5)
    assert(connectedLists.head.contains(Point(2, 10)))
    assert(connectedLists.last.contains(Point(2, 10)))
  }

  test("Given a list of Boxes " +
    " List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 11), Point(3, 11), Point(3, 12))" +
    "and List(Point(2, 9), (2, 11), (3, 11), (3, 12))" +
    "and a given point Point(100,100), calling " +
    "addConnection() should create a new sublist with Point(100,100) because" +
    "from both the lists, the point Point(2,10) can't be connected") {

    val listOfBoxes: List[Box] = List(
      List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 11), Point(3, 11), Point(3, 12)),
      List(Point(2, 9), Point(2, 11), Point(3, 11), Point(3, 12))
    )
    val givenPoint = Point(100, 100)

    val connectedLists: List[Box] = addConnection(listOfBoxes, givenPoint)

    assert(connectedLists.size == 3)
    assert(connectedLists.head.length == 6)
    assert(connectedLists.last.length == 1)
    assert(connectedLists.head.contains(Point(1, 10)))
    assert(connectedLists.last.contains(Point(100, 100)))
  }

  test("Given an empty list " +
    "and a given point Point(2,10), calling addConnection should create a new list with " +
    "the given point Point(2,10) and added to the lists of lists") {

    val listOfBoxes: List[Box] = Nil
    val givenPoint = Point(2, 10)

    val connectedLists: List[Box] = addConnection(listOfBoxes, givenPoint)
    assert(connectedLists.size == 1)
    assert(connectedLists.head.length == 1)
    assert(connectedLists.head.contains(Point(2, 10)))
  }

  // test for connect
  test("Given a list of boxes (i.e lists of points) " +
    " Box1: List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2,10), Point(2, 11))" +
    "and Box2: List(Point(2, 9), Point(2,10), Point(2, 11))" +
    "and a given line -----***--** and a row number 2, calling " +
    "connect() should add Point(3,11) and Point(3,12) to the first box (sublist) and" +
    "Point(3,11) and Point(3,12) to the second box (sublist). Also it would create a new box (sublist)" +
    "for Point(3,6), Point(3,7) and Point(3,8)") {

    val listOfBoxes: List[Box] = List(
      List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11)),
      List(Point(2, 9), Point(2, 10), Point(2, 11))
    )
    val givenLine = "-----***--**"

    val connectedLists: List[Box] = connect(listOfBoxes, givenLine, 2)
    assert(connectedLists.size == 3)
    assert(connectedLists.head.length == 7)
    assert(connectedLists(1).length == 5)
    assert(connectedLists.head.contains(Point(3, 11)))
    assert(connectedLists.head.contains(Point(3, 12)))
    assert(connectedLists(1).contains(Point(3, 11)))
    assert(connectedLists(1).contains(Point(3, 12)))
    assert(connectedLists.last.forall(p => p == Point(3, 6) || p == Point(3, 7) || p == Point(3, 8)))
  }

  // build connected list
  test("Given a list of strings List(**-------***, -*--**--***-, -----***--**, -------***--) and" +
    "calling buildConnectedLists() would return list of boxes (i.e list of lists of points)") {
    val list = List("**-------***", "-*--**--***-", "-----***--**", "-------***--")
    val listOfBoxes: List[Box] = buildConnectedBoxes(list)
    val expectedOutput: List[Box] = List(
      List(Point(1, 1), Point(1, 2), Point(2, 2)),
      List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12)),
      List(Point(2, 5), Point(2, 6), Point(3, 6), Point(3, 7), Point(3, 8), Point(4, 8), Point(4, 9), Point(4, 10)),
      List(Point(2, 9), Point(2, 10), Point(2, 11), Point(3, 11), Point(3, 12))
    )
    val listOfBoxesSet = listOfBoxes.map(_.toSet)
    val expectedOutputSet = expectedOutput.map(_.toSet)
    assert(listOfBoxesSet == expectedOutputSet)
  }

  test("Given a list of strings List(**-------***, -*--**--***-) and" +
    "calling buildConnectedLists() would return list of boxes (i.e list of lists of points)") {
    val list = List("**-------***", "-*--**--***-")
    val listOfBoxes: List[Box] = buildConnectedBoxes(list)
    val expectedOutput: List[Box] = List(
      List(Point(1, 1), Point(1, 2), Point(2, 2)),
      List(Point(1, 10), Point(1, 11), Point(1, 12), Point(2, 10), Point(2, 11)),
      List(Point(2, 5), Point(2, 6)),
      List(Point(2, 9), Point(2, 10), Point(2, 11))
    )
    assert(listOfBoxes == expectedOutput)
  }
}
