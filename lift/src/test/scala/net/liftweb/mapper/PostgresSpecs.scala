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
        
          Schemifier.destroyTables_!!(ignoreLogger _, SampleModel)
          Schemifier.schemify(true, ignoreLogger _, SampleModel)
        
          val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).open_!
          val madeline = SampleModel.find(By(SampleModel.firstName, "Madeline")).open_!
          val archer = SampleModel.find(By(SampleModel.firstName, "Archer")).open_!
        
          elwood.firstName.toString must_== "Elwood"
          madeline.firstName.toString must_== "Madeline"
          archer.firstName.toString must_== "Archer"
        
          elwood.id.is must be_<(madeline.id.is)
        }
     }
   }

   private def ignoreLogger(f: => AnyRef): Unit = ()
} 

object SampleModel extends SampleModel with KeyedMetaMapper[Long, SampleModel] {
  override def dbAddTable = Full(populate _)
  
  private def populate {
    create.firstName("Elwood").save
    create.firstName("Madeline").save
    create.firstName("Archer").save
  }  
}

class SampleModel extends KeyedMapper[Long, SampleModel] {
  def getSingleton = SampleModel // what's the "meta" server
  def primaryKeyField = id

object id extends MappedLongIndex(this)
  
  // First Name
  object firstName extends MappedString(this, 32)  
}
