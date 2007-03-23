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
import java.sql.{Connection, DriverManager}
import java.io.File
import scala.actors._

trait Runner {
  def name: String
  def setupDB: unit
}

object TestRunner {
  def main(arg: Array[String]) {
    val dbRunners = DerbyRunner :: MySqlRunner :: Nil
    
    val totalTime = calcTime {
      val r = new TestResult
      val suite = new TestSuite
      val userTests = new UserTests
      val stateMachineTests = new StateMachineTests
      var addedUserTests = false
      
      suite.addTest(new RegExTests)
      suite.addTest(new HelperTests)
      
      if (true) {
      dbRunners.foreach {
        runner =>
      Console.println("Test runner for..."+runner.name)
      if (!addedUserTests) {
        suite.addTest(userTests)
        suite.addTest(stateMachineTests)
        addedUserTests = true
      }
      runner.setupDB

      val ut = new UserTests
      ut.init

      suite.run(r)
      Scheduler.shutdown
      } }
      else {
        suite.run(r)
      }
      Scheduler.shutdown


      for (val tf <- r.failures()) {
        Console.println(tf.toString())
        Console.println(tf.trace)
      }
      Console.println(r.failures.toList.length+" Failures")
    }
    
    Console.println("It took "+totalTime+" to run the tests")
  }
  
  }

object DerbyRunner extends Runner {
  def name = "Derby"
    
  def setupDB {
    val f = new File("lift_tests")
    
    def deleteIt(file: File) {
      if (file.exists) {
        if (file.isDirectory) file.listFiles.foreach{f => deleteIt(f)}
        file.delete
      }
    }
    
    deleteIt(f)

    DB.defineConnectionManager("", DBVendor)
    
    Schemifier.schemify(User, Pet, TestStateMachine)
    // Schemifier.schemify(User, Pet)
  }  
}

object MySqlRunner extends Runner {
  def name = "MySql"
    
  def setupDB {
    DB.defineConnectionManager("", MySQLVendor)
    
    def deleteAllTables {
    DB.use {
      conn =>
      val md = conn.getMetaData
      val rs = md.getTables(null, null, null, null)
      var toDelete: List[String] = Nil
      while (rs.next) {
        val tableName = rs.getString(3)
        if (rs.getString(4).toLowerCase == "table") toDelete = tableName :: toDelete
      }
      rs.close
      
      toDelete.foreach {
        table =>
        try {
        val ct = "DROP TABLE "+table
        val st = conn.createStatement
        st.execute(ct)
        st.close
        } catch {
          case e => e.printStackTrace
        }
      }
      
      if (toDelete.length > 0) deleteAllTables
    }
    }
    deleteAllTables
    
    Schemifier.schemify(User, Pet, TestStateMachine)
    // Schemifier.schemify(User, Pet)
  }
}

object MySQLVendor extends ConnectionManager {
  def newConnection(name: String): Option[Connection] = {
    try {
      Class.forName("com.mysql.jdbc.Driver")
      
      val dm =  DriverManager.getConnection("jdbc:mysql://localhost:3306/lift_test?autoReconnect=true", "dpp", "")
      Some(dm)
    } catch {
      case e : Exception => e.printStackTrace; None
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
