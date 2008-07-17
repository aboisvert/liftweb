package net.liftweb.util
import org.specs._
import org.specs.runner._
import scala.xml._

object BindHelpersSpec extends Specification with BindHelpers {
  "the chooseTemplate function" should {
    "select the node matching a given tag and prefix" in {
      chooseTemplate("choose", "tag", <h><choose:tag a="att1">that</choose:tag></h>) must ==/(Text("that"))
    }
    "select the first node matching a given tag and prefix" in {
      chooseTemplate("choose", "tag", <h><choose:tag>that</choose:tag><choose:tag>those</choose:tag></h>) must ==/(Text("that"))
    }
    "return an empty NodeSeq if no node is found" in {
      chooseTemplate("choose", "tag", <h></h>) must be_==(NodeSeq.Empty)
    }
  }
  "the bind(Map, NodeSeq) function" should {
    "replace the content of a lift:bind node with the content of a map where the key is the value of the attribute 'name'" in {
      val map = Map("hello" -> <h1></h1>, "world" -> <b></b>)
      val liftbind = <body>
        <lift:bind name="hello">changethis</lift:bind>
     </body>
      bind(map, liftbind) must ==/(<body><h1></h1></body>)
    }
  }
  "the bindlist function" should {
    "replace the content of a lift:bind node with the content of a map where the key is the value of the attribute 'name'" in {
      val maps = List(Map("hello" -> <h1></h1>, "world" -> <b></b>))
      val liftbind = <body>
        <lift:bind name="hello">changethis</lift:bind>
     </body>
      bindlist(maps, liftbind).get must ==/(<body><h1></h1></body>)
    }
  }
  "the bind(namespace, NodeSeq, BindParams*) function" should {
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the String value of the BindParam" in {
      bind("user", <t><user:tag>replacethis</user:tag></t>, "tag" --> "world") must ==/(<t>world</t>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the Symbol value of the BindParam" in {
      bind("user", <t><user:tag>replacethis</user:tag></t>, "tag" --> 'world) must ==/(<t>world</t>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the NodeSeq value of the BindParam" in {
      bind("user", <user:tag>replacethis</user:tag>, "tag" --> <world></world>) must ==/(<world></world>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the NodeSeq value of the BindParam" in {
      bind("user", <user:tag>replacethis</user:tag>, "tag" --> <world></world>) must ==/(<world></world>)
    }
    "replace a node named 'namespace:bindparam name' in a NodeSeq with the function application of a FuncBindParam" in {
      bind("user", <t><user:tag>hello</user:tag></t>, FuncBindParam("tag", (n: NodeSeq) => Text(n.text + " world"))) must ==/(<t>hello world</t>)
    }
    "replace an attribute value named 'namespace:bindparam name' in a NodeSeq with a value from a BindParam" in {
      bind("user", <t user:hello="toreplace"></t>, "hello" --> Text("world")) must ==/(<t user:hello="world"></t>)
    }
    "replace an attribute value named 'namespace:bindparam name' in a NodeSeq with a calculated value from a FuncBindParam" in {
      bind("user", <t user:tag="hello"></t>, FuncBindParam("tag", (n: NodeSeq) => Text(n.text + " world"))) must ==/(<t user:tag="hello world"></t>)
    }
    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a new attribute name and value from an AttrBindParam" in {
      bind("user", <t user:tag="toreplace"></t>, AttrBindParam("tag", Text("world"), "hello")) must ==/(<t hello="world"></t>)
    }
    "replace an attribute named 'namespace:bindparam name' in a NodeSeq with a new attribute name and calculated value from an FuncAttrBindParam" in {
      bind("user", <t user:tag="dear"></t>, FuncAttrBindParam("tag", (n: NodeSeq) =>Text(n.text + " world"), "hello")) must ==/(<t hello="dear world"></t>)
    }
  }
  "the xmlParam function" should {
    "find the value of an attribute in an xml fragment" in {
      xmlParam(<t hello="world">world</t>, "hello") must_== Full("world")
    }
    "return Empty if the value is empty" in {
      xmlParam(<t hello="">world</t>, "hello") must_== Empty
    }
    "return Empty if the attribute is not found" in {
      xmlParam(<t hello="">world</t>, "notfound") must_== Empty
    }
  }
}
class BindHelpersTest extends JUnit4(BindHelpersSpec)
 