package net.liftweb.tests

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.util.Helpers
import Helpers._
import scala.testing.SUnit
import SUnit._
import net.liftweb.mapper._
import net.liftweb.proto.ProtoUser
import java.sql.{Connection, DriverManager}
import java.io.File

object TestRunner {
  def main(arg: Array[String]) {
  val totalTime = calcTime {
    Console.println("Test runner...")
    val r = new TestResult
    val suite = new TestSuite

    setupDB
    
    suite.addTest(new RegExTests)
    suite.addTest(new UserTests)
    suite.addTest(new HelperTests)
    suite.run(r)
 for (val tf <- r.failures()) {
   Console.println(tf.toString())
   Console.println(tf.trace)
 }
    Console.println(r.failures.toList.length+" Failures")
  }
  
  Console.println("It took "+totalTime+" to run the tests")
  }
  
  def setupDB {
    val f = new File("lift_tests")
    
    def deleteIt(file: File) {
      if (file.exists) {
        if (file.isDirectory) file.listFiles.foreach{f => deleteIt(f)}
        file.delete
      }
    }
    
    deleteIt(f)

    DB.connectionManager = Some(DBVendor)
    
    DB.use {
      db =>
      val st = db.createStatement
      // create the table
      st.execute("CREATE TABLE users (id INT NOT NULL GENERATED ALWAYS AS IDENTITY, firstname VARCHAR(64), lastname VARCHAR(64), email VARCHAR(64), password_slt VARCHAR(64), password_pw VARCHAR(64))")
    }
  }
}

object DBVendor extends ConnectionManager {
  def newConnection(name: String): Option[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      
      val dm =  DriverManager.getConnection("jdbc:derby:lift_tests;create=true")
      Some(dm)
    } catch {
      case e : Exception => e.printStackTrace; None
    }
  }
}