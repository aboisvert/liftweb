/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.mapper

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import Helpers._
import java.sql.{Connection, DriverManager}

//import net.liftweb.mapper.DBVendors.{MySqlRunner, DerbyRunner}

class MapperSpecsAsTest extends JUnit3(MapperSpecs)
object MapperSpecsRunner extends ConsoleRunner(MapperSpecs)

object MapperSpecs extends Specification {
  def providers = DBProviders.asList
   
  providers.foreach(provider => {
    ("Mapper for " + provider.name) should {

      "schemify" in {
        try { provider.setupDB } catch { case e => skip(e.getMessage) }
        
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
 })

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
  object firstName extends MappedString(this, 32)  
}
