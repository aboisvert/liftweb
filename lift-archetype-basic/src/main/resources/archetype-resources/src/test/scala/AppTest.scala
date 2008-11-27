package ${packageName}

import _root_.java.io.File
import _root_.junit.framework._
import Assert._
import _root_.scala.xml.XML

object AppTest {
  def suite: Test = {
    val suite = new TestSuite(classOf[AppTest])
    suite
  }

  def main(args : Array[String]) {
    _root_.junit.textui.TestRunner.run(suite)
  }
}

/**
 * Unit test for simple App.
 */
class AppTest extends TestCase("app") {
  /**
   * Rigourous Tests :-)
   */
  def testOK() = assertTrue(true)
  //def testKO() = assertTrue(false)

  /**
   * Tests to make sure the project's XML files are well-formed.
   * 
   * Finds every *.html and *.xml file in src/main/webapp (and its
   * subdirectories) and tests to make sure they are well-formed.
   */
  def testXml() = {
    var failed: List[File] = Nil
    
    def wellFormed(file: File) {
      if (file.isDirectory)
        for (f <- file.listFiles) wellFormed(f)

      if (file.isFile && (file.getName.endsWith(".html") || file.getName.endsWith(".xml"))) {
        try {
          XML.loadFile(file)
        } catch {
          case e: org.xml.sax.SAXParseException => failed = file :: failed
        }
      }
    }

    wellFormed(new File("src/main/webapp"))
    
    val numFails = failed.size
    if (numFails > 0) {
      val fileStr = if (numFails == 1) "file" else "files"
      fail("Malformed XML in " + numFails + " files: " + failed.mkString(", "))
    }
  }
}
