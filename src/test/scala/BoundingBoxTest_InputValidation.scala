import org.scalatest.funsuite.AnyFunSuite
import BoundingBoxHelpers._
import BoundingBox._
import BoxDomain._

class BoundingBoxTest_InputValidation extends AnyFunSuite {
  test("Bounding box inputs which have empty input should return false for doesInputHaveSameColumnLength() ") {
    assert(!doesInputHaveSameColumnLength(Nil))
  }

  test("Bounding box inputs which have only one line of input " +
    "should produce true for doesInputHaveSameColumnLength()") {
    val list: List[String] = List("*---*")
    assert(doesInputHaveSameColumnLength(list))
  }

  test("Bounding box inputs which have multiple lines of input " +
    "but with varying column lengths should produce false for doesInputHaveSameColumnLength () ") {
    val list: List[String] = List("*---*", "***")
    assert(!doesInputHaveSameColumnLength(list))
  }

  test("Bounding box inputs which have multiple lines of input with same column lengths " +
    "should produce true for doesInputHaveSameColumnLength ") {
    val list: List[String] = List("*---*", "*****", "----*")
    assert(doesInputHaveSameColumnLength(list))
  }


  test("Bounding box inputs which have multiple lines of input with chars only '*' and '-' " +
    "should produce true for doesInputHaveOnlyHyphenAndAsterisk ") {
    val list: List[String] = List("*---*", "*****", "----*")
    assert(doesInputHaveOnlyHyphenAndAsterisk(list))
  }

  test("Bounding box inputs which have multiple lines of input with space chars " +
    "should produce false for doesInputHaveOnlyHyphenAndAsterisk ") {
    val list: List[String] = List("*---*", "     ", "----*")
    assert(!doesInputHaveOnlyHyphenAndAsterisk(list))
  }

  test("Bounding box inputs which have multiple lines of input with different chars " +
    "other than '*' and '-' should produce false for doesInputHaveOnlyHyphenAndAsterisk ") {
    val list: List[String] = List("*---*", "abcde", "----*")
    assert(!doesInputHaveOnlyHyphenAndAsterisk(list))
  }

}

