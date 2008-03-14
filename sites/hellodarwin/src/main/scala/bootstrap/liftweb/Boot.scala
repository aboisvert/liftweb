package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("sandbox.lift.hellodarwin")

    // Build SiteMap
    val entries = Menu(Loc("Home", "/", "Home")) ::
      Menu(Loc("Hello1.1", "/helloStatic", "Hello Static")) ::
      Menu(Loc("Hello1.1", "/helloStatic", "Hello Static")) ::
      Menu(Loc("Hello1.2", "/helloStatic2", "Hello Static2")) ::
      Menu(Loc("Hello1.3", "/helloStatic3", "Hello Static3")) ::
      Menu(Loc("Hello1.4", "/helloStatic4", "Hello Static4")) ::
      Menu(Loc("Hello2.1", "/helloSnippet", "Hello Snippet")) ::
      Menu(Loc("Hello2.2", "/helloSnippet2", "Hello Snippet2")) ::
      Menu(Loc("Hello3.1", "/helloForm", "Hello Form")) ::
      Menu(Loc("Hello3.2", "/helloForm2", "Hello Form2")) ::
      Menu(Loc("Hello3.3", "/helloForm3", "Hello Form3")) ::
      Menu(Loc("Hello3.4", "/helloForm4", "Hello Form4")) ::
      Menu(Loc("Hello4.1", "/helloFormAjax", "Hello FormAjax")) ::
      Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))
  }
}

