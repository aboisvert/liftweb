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
  "the bind function" should {
    "map attribute name to xml nodes in a lift:bind node" in {
      val map = Map("hello" -> <h1></h1>, "world" -> <b></b>)
      val liftbind = <lift:bind><node1 name="hello"/></lift:bind>
      bind(map, liftbind) must_== <e></e>
    }
  }
}
class BindHelpersTest extends JUnit4(BindHelpersSpec)
