package net.liftweb.tests

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import _root_.net.liftweb.util.{Helpers, Log, Can, Empty, Full, Failure}
import Helpers._
import _root_.scala.testing.SUnit
import SUnit._
import _root_.net.liftweb.mapper._
import _root_.java.sql.{Connection, DriverManager}
import _root_.java.io.File
import _root_.scala.actors._

trait Runner {
  def name: String
  def setupDB: unit
}

object TestRunner {

  def main(arg: Array[String]) {
    val cmdLineRunners = arg.map {
      case "derby" => DerbyRunner
      case "mysql" => MySqlRunner
      case "postgresql" => PostgreSqlRunner
      case "h2" => H2Runner
      case "h2memory" => H2MemoryRunner
    }.toList

    val dbRunners = cmdLineRunners match {
      case x :: xs => x :: xs
      case _ => H2Runner :: H2MemoryRunner :: DerbyRunner :: MySqlRunner :: PostgreSqlRunner :: Nil
    }

    Log.info("Runners: " + dbRunners)

    val totalTime = calcTime {
      val r = new TestResult
      val suite = new TestSuite
      val userTests = new UserTests
      val stateMachineTests = new StateMachineTests
      var addedUserTests = false

      suite.addTest(new RegExTests)
      suite.addTest(new HelperTests)

      if (true) {
        DB.addLogFunc((s, l) => Log.info("query: " + s + ", time: " + l + " ms."))
        dbRunners.foreach {
          runner => calcTime {
            Log.info("Test runner for..."+runner.name)
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
            runner.name
          } match { case (time, name) => Log.info("Test runner for..."+name + " finished in " + time + " ms.") }
        }
      }
      else {
        suite.run(r)
      }
      Scheduler.shutdown


      for (tf <- r.failures()) {
        Log.error(tf.toString())
        Log.error(tf.trace)
      }
      Log.info(r.failures.toList.length+" Failures")
    }

    Log.info("It took "+totalTime+" to run the tests")
  }
}

trait FileDbSetup {
  def filePath : String
  def vendor : Vendor

  def setupDB {
    val f = new File(filePath)

    def deleteIt(file: File) {
      if (file.exists) {
        if (file.isDirectory) file.listFiles.foreach{f => deleteIt(f)}
        file.delete
      }
    }

    deleteIt(f)

    DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)

    Schemifier.schemify(true, trialLog _, User, Pet)
    Schemifier.schemify(true, trialLog _, TestStateMachine)
  }

  def trialLog(s: => AnyRef) {
    Log.info(s)
  }
}

trait DbSetup {
  def vendor : Vendor

  def trialLog(s: => AnyRef) {
    Log.info(s)
  }

  def setupDB {
    DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)

    def deleteAllTables {
      DB.use(DefaultConnectionIdentifier) {
        conn =>
          val md = conn.getMetaData
          val rs = md.getTables(null, Schemifier.getDefaultSchemaName(conn), null, null)
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

    Schemifier.schemify(true,trialLog _, User, Pet, TestStateMachine)
  }
}

abstract class Vendor(driverClass : String) extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection] = {
    try {
      Class.forName(driverClass)
      Full(mkConn)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }

  def releaseConnection(conn: Connection) {conn.close}

  def mkConn : Connection
}

object MySqlRunner extends Runner with DbSetup {
  def name = "MySql"
  def vendor = new Vendor("com.mysql.jdbc.Driver") {
    def mkConn = DriverManager.getConnection("jdbc:mysql://localhost:3306/lift_test?autoReconnect=true", "dpp", "")
  }
}

object PostgreSqlRunner extends Runner with DbSetup {
  def name = "PostgreSql"
  def vendor = new Vendor("org.postgresql.Driver") {
    def mkConn = DriverManager.getConnection("jdbc:postgresql://localhost/lift", "lift", "lift")
  }
}

object DerbyRunner extends Runner with FileDbSetup {
  def name = "Derby"
  def filePath = "lift_tests"
  def vendor = new Vendor("org.apache.derby.jdbc.EmbeddedDriver") {
    def mkConn = DriverManager.getConnection("jdbc:derby:" + filePath + ";create=true")
  }
}

object H2Runner extends Runner with FileDbSetup {
  def name = "H2"
  def filePath = "h2_lift_tests"
  def vendor = new Vendor("org.h2.Driver") {
    def mkConn = DriverManager.getConnection("jdbc:h2:" + filePath + "/test.db")
  }
}

object H2MemoryRunner extends Runner with DbSetup {
  def name = "H2 in memory"
  def vendor = new Vendor("org.h2.Driver") {
    def mkConn = DriverManager.getConnection("jdbc:h2:mem:lift;DB_CLOSE_DELAY=-1")
  }
}
