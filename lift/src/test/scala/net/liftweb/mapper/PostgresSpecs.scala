package net.liftweb.mapper

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import Helpers._
import java.sql.{Connection, DriverManager}

class PosgresTest extends JUnit3(PostgresSpecs)
object PostgresSpecsRunner extends ConsoleRunner(PostgresSpecs)

object PostgresSpecs extends Specification {
  
   "Postgres Driver" should {
     "schemify" in {
        val driver = tryo {
          val cls = Class.forName("org.postgresql.Driver")
           DriverManager.getConnection("jdbc:postgresql://localhost/lift_test", "lift", "")
        }
                
        driver.foreach{d =>
        DB.defineConnectionManager(DefaultConnectionIdentifier, new ConnectionManager {
          def newConnection(name: ConnectionIdentifier): Can[Connection] = Full(d)
          def releaseConnection(conn: Connection) = () // {conn.close}
        })
        
        Schemifier.destroyTables_!!(ignoreLogger _, TestModel)
        Schemifier.schemify(true, ignoreLogger _, TestModel)
        
        val elwood = TestModel.find(By(TestModel.firstName, "Elwood")).open_!
          val madeline = TestModel.find(By(TestModel.firstName, "Madeline")).open_!
            val archer = TestModel.find(By(TestModel.firstName, "Archer")).open_!
        
              elwood.firstName.toString must verify((s: String) => s == "Elwood")
        madeline.firstName.toString must verify((s: String) => s == "Madeline")
        archer.firstName.toString must verify((s: String) => s == "Archer")
        
        elwood.id.is must verify((id: Long) => id < madeline.id.is)
        }
     }
   }

   private def ignoreLogger(f: => AnyRef): Unit = ()
} 

object TestModel extends TestModel with KeyedMetaMapper[Long, TestModel] {
  override def dbAddTable = Full(populate _)
  
  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").save
  }  
}

class TestModel extends KeyedMapper[Long, TestModel] {
  def getSingleton = TestModel // what's the "meta" server
  def primaryKeyField = id

object id extends MappedLongIndex(this)
  
  // First Name
  object firstName extends MappedString(this, 32)  
}
