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

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.liftweb.util._
import Helpers._
import _root_.java.sql.{Connection, DriverManager}

//import _root_.net.liftweb.mapper.DBVendors.{MySqlRunner, DerbyRunner}

class MapperSpecsAsTest extends JUnit3(MapperSpecs)
object MapperSpecsRunner extends ConsoleRunner(MapperSpecs)

object MapperSpecs extends Specification {
  def providers = DBProviders.asList

  providers.foreach(provider => {
    ("Mapper for " + provider.name) should {

      "schemify" in {
        try { provider.setupDB } catch { case e => skip(e.getMessage) }

        Schemifier.destroyTables_!!(ignoreLogger _, SampleModel, SampleTag)
        Schemifier.schemify(true, ignoreLogger _, SampleModel, SampleTag)

        val elwood = SampleModel.find(By(SampleModel.firstName, "Elwood")).open_!
        val madeline = SampleModel.find(By(SampleModel.firstName, "Madeline")).open_!
        val archer = SampleModel.find(By(SampleModel.firstName, "Archer")).open_!

        elwood.firstName.is must_== "Elwood"
        madeline.firstName.is must_== "Madeline"
        archer.firstName.is must_== "Archer"

	val meow = SampleTag.find(By(SampleTag.tag, "Meow")).open_!

	meow.tag.is must_== "Meow"

        elwood.id.is must be_<(madeline.id.is)
      }

      "Like works" in {
        try { provider.setupDB } catch { case e => skip(e.getMessage) }

        Schemifier.destroyTables_!!(ignoreLogger _, SampleModel, SampleTag)
        Schemifier.schemify(true, ignoreLogger _, SampleModel, SampleTag)

	val oo = SampleTag.findAll(Like(SampleTag.tag, "%oo%"))

	(oo.length > 0) must beTrue

	for (t <- oo)
	  (t.tag.is.indexOf("oo") >= 0) must beTrue

	for (t <- oo)
	  t.model.cached_? must beFalse

	val mm = SampleTag.findAll(Like(SampleTag.tag, "M%"))

	(mm.length > 0) must beTrue

	for (t <- mm)
	  (t.tag.is.startsWith("M")) must beTrue

	for (t <- mm) {
	  t.model.cached_? must beFalse
	  t.model.obj
	  t.model.cached_? must beTrue
	}

      }

      "Precache works" in {
        try { provider.setupDB } catch { case e => skip(e.getMessage) }

        Schemifier.destroyTables_!!(ignoreLogger _, SampleModel, SampleTag)
        Schemifier.schemify(true, ignoreLogger _, SampleModel, SampleTag)


	val oo = SampleTag.findAll(By(SampleTag.tag, "Meow"),
				   PreCache(SampleTag.model))

	(oo.length > 0) must beTrue

	for (t <- oo)
	  t.model.cached_? must beTrue
      }
    }
 })

 private def ignoreLogger(f: => AnyRef): Unit = ()
}

object SampleTag extends SampleTag with LongKeyedMetaMapper[SampleTag] {
  override def dbAddTable = Full(populate _)
  private def populate {
    val samp = SampleModel.findAll()
    val tags = List("Hello", "Moose", "Frog", "WooHoo", "Sloth",
		    "Meow", "Moof")
    for (t <- tags;
	 m <- samp) SampleTag.create.tag(t).model(m).save
  }
}

class SampleTag extends LongKeyedMapper[SampleTag] with IdPK {
  def getSingleton = SampleTag // what's the "meta" server

  object tag extends MappedString(this, 32)
  object model extends MappedLongForeignKey(this, SampleModel)
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
