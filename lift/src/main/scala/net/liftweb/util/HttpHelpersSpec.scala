package net.liftweb.util
import org.specs._
import org.specs.specification._

object HttpHelpersSpec extends Specification with HttpHelpers with ListHelpers with StringHelpers {

  "Http helpers" should provide {
    "urlEncode and urlDecode functions" in {
      urlDecode(urlEncode("hello world")) must_== "hello world"      
      urlEncode(urlDecode("hello+world")) must_== "hello+world"      
    }
    "a paramsToUrlParams function to translate a map of parameters to a URL query" in {
      paramsToUrlParams(List(("firstname", "hello"), ("lastname", "world"))) must_== "firstname=hello&lastname=world"
    }
    "an appendParams function to add parameters to a URL query" in {
      "creating the param list with ? if there are no existing params" >> {
        appendParams("www.helloworld.com/params", List(("firstname", "hello"), ("lastname", "world"))) must_== 
        "www.helloworld.com/params?firstname=hello&lastname=world"
      } 
      "appending the param list with & if there are some already" >> {
        appendParams("www.helloworld.com/params?firstname=hello", List(("lastname", "world"))) must_== 
        "www.helloworld.com/params?firstname=hello&lastname=world"
      } 
      "returning the url if no param list is passed" >> {
        appendParams("www.helloworld.com/params", Nil) must_== "www.helloworld.com/params"
      } 
    }
  }
  def provide(e: =>Example) = { currentSut.verb += " provide"; e }
}
class HttpHelpersSpecTest extends org.specs.runner.JUnit4(HttpHelpersSpec)