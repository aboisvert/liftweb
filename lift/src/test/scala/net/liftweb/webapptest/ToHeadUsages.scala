package net.liftweb.webapptest

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.sourceforge.jwebunit.junit.WebTester

class ToHeadUsagesTest extends JUnit3(ToHeadUsages)
object ToHeadUsagesRunner extends ConsoleRunner(ToHeadUsages)


object ToHeadUsages extends Specification {
  JettyTestServer.start()

  "lift <head> merger" should {
    "merge <head> from html fragment" >> {
      JettyTestServer.browse(
        "/htmlFragmentWithHead",
        _.assertElementPresentByXPath("/html/head/script[@id='fromFrag']")
      ) 
    }

    "merge <head> from snippet" >> {
      JettyTestServer.browse(
        "/htmlSnippetWithHead",
        _.assertElementPresentByXPath("/html/head/script[@src='snippet.js']")
      )
    }
  }

  JettyTestServer.stop()
}
