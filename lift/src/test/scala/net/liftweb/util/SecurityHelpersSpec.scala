package net.liftweb.util
import org.specs._
object SecurityHelpersSpec extends Specification with SecurityHelpers with IoHelpers with StringHelpers {
  "Security Helpers" should {
    "provide a randomLong method returning a random Long modulo a number" in {
      randomLong(7L) must be_<(7L)
    }
    "provide a randomInt method returning a random Int modulo a number" in {
      randomInt(7) must be_<(7)
    }
  }
}
import org.specs.runner._
class SecurityHelpersSpecTest extends JUnit4(SecurityHelpersSpec)
