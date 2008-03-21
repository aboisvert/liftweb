package net.liftweb.util
import org.specs.runner._
import org.specs._

class ClassHelpersSpecTest extends Runner(ClassHelpersSpec) with JUnit
object ClassHelpersSpec extends Specification with ClassHelpers with ControlHelpers {
  "the findClass function" should {
    "return a Full can with the found class when given the name and package" in {
      findClass("ClassHelpersSpecTest", List("net.liftweb.util")) must_== Full(classOf[ClassHelpersSpecTest])
    }
    "return a Full can with the found class when given the name and package, with an underscored name instead of CamelCased" in {
      findClass("class_helpers_spec_test", List("net.liftweb.util")) must_== Full(classOf[ClassHelpersSpecTest])
    }
    "return a Full can with the found class when given the name and a list of packages" in {
      findClass("ClassHelpersSpecTest", List("net.liftweb.util", "other.package")) must_== Full(classOf[ClassHelpersSpecTest])
    }
    "return a Full can with the found class when given the name, a list of packages and a target type to conform to" in {
      findClass("ArrayList", List("java.util"), Full(classOf[java.util.List[Object]])) must_== Full(classOf[java.util.ArrayList[Object]])
    }
    "return an Empty can if no class is found given a name and package" in {
      findClass("ClassHelpersSpecTest", List("net.liftweb.nothere")) must_== Empty
    }
    "return an Empty can if the class cannot be coerced to the expected type" in {
      findClass("ClassHelpersSpecTest", List("net.liftweb.util"), Full(classOf[String])) must_== Empty
    }
  }
  "the findClass function" can {
    "return a Full can with the found class when given a list of names and corresponding packages" in {
      findClass(List(("wrong name", List("net.liftweb.util", "other.package")),
                     ("ClassHelpersSpecTest", List("net.liftweb.util", "other.package")))) must_== Full(classOf[ClassHelpersSpecTest])
    }
    "use a list of modifiers functions to try to modify the original name in order to find the class" in {
      findClass("classHelpersSpecTest", List("net.liftweb.util"), List((n: String) => n.capitalize)) must_== Full(classOf[ClassHelpersSpecTest])
    }
  }
}
