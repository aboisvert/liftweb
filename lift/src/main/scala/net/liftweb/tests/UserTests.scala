package net.liftweb.tests

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.testing.SUnit
import SUnit._

import net.liftweb.util.RE
import net.liftweb.util.RE._
import net.liftweb.mapper._
import net.liftweb.proto._

class UserTests extends TestCase("User Tests") {
  val maxUsers = 100
  override def runTest() {
    DB.use {
      conn =>
	for (val cnt <- 1 to maxUsers) {
	  val u = new User
	  u.firstName := cnt.toString
	  u.lastName := "Name "+cnt
	  u.email := "mr"+cnt+"@foo.com"
	  u.password := "password"+cnt
	  u.save
	  for (val petCnt <- 1 to (1 + cnt/ 10)) {
            val p = new Pet
            p.name := ""+petCnt+" of "+u.lastName
            p.owner := u.id.get
            p.save
	  }
	}

      findTest
      findAllTest
      countTest
      pwdTest
    }
  }
  
  def findTest {
    assert(User.find(1).isDefined)
    assert(User.find(ByField(User.email, "mr9@foo.com")).isDefined)    
    assert(!User.find(ByField(User.email, "eemr1@foo.com")).isDefined)  
    assert(User.find(BySql("email = ?", "mr9@foo.com")).isDefined)
    assert(!User.find(BySql("email = ?", "eemr1@foo.com")).isDefined)  
    assert(User.find(BySql("email = ?", "mr9@foo.com"), BySql("firstname = ?", "9")).isDefined)  
    assert(User.find(BySql("email = ? AND firstname = ?", "mr9@foo.com", "9")).isDefined)  
    assert(!User.find(BySql[User]("email = ? AND firstname = ?", "mr1@foo.com", "33")).isDefined)  
    val u = User.find(33).get
    assert(User.find(BySql("email = ?", u.email)).get.id.get == u.id.get)
    assert(User.find(BySql("id = ?", 33)).get.id.get == u.id.get)
    assert(User.find(BySql("id = ?", u.id)).get.id.get == u.id.get)
    
    for (val uKey <- 1 to maxUsers) {
      val u = User.find(uKey)
      assert(u.isDefined)
      val user = u.get
      assert(user.pets.length == (1 + uKey / 10))
    }
  }
  
  def findAllTest {
    assert(User.findAll.length == maxUsers)
    assert(User.findAll(ByField(User.email, "mr33@foo.com")).length == 1)
    assert(User.findAll(ByField(User.email, "dogmr33@foo.com")).length == 0)
    assert(User.findAll(BySql("email = ?", "mr9@foo.com")).length == 1)
    assert(User.findAll(BySql("email = ?", "eemr1@foo.com")).length == 0)  
    assert(User.findAll(BySql("email = ?", "mr9@foo.com"), BySql("firstname = ?", "9")).length == 1)  
    assert(User.findAll(BySql("email = ? AND firstname = ?", "mr9@foo.com", "9")).length == 1)  
    assert(User.findAll(BySql[User]("email = ? AND firstname = ?", "mr1@foo.com", "33")).length == 0)  
    val u = User.find(33).get
    assert(User.findAll(BySql("email = ?", u.email)).length == 1)
    assert(User.findAll(OrderBy(User.firstName, true))(0).firstName.get == "1")
    assert(User.findAll(OrderBy(User.firstName, false))(0).firstName.get == "99")
  }
  
  def countTest {
    assert(User.count == maxUsers)
    
    assert(User.count(ByField(User.email, "mr33@foo.com")) == 1)
    assert(User.count(ByField(User.email, "dogmr33@foo.com")) == 0)
    assert(User.count(BySql("email = ?", "mr9@foo.com")) == 1)
    assert(User.count(BySql("email = ?", "eemr1@foo.com")) == 0)  
    assert(User.count(BySql("email = ?", "mr9@foo.com"), BySql("firstname = ?", "9")) == 1)  
    assert(User.count(BySql("email = ? AND firstname = ?", "mr9@foo.com", "9")) == 1)  
    assert(User.count(BySql("email = ? AND firstname = ?", "mr1@foo.com", "33")) == 0)  
    assert(User.count(BySql("email = ? AND firstname = ?", "mr1@foo.com", "1")) == 1)  
    val u = User.find(33).get
    assert(User.count(BySql("email = ?", u.email)) == 1)
    assert(User.count(BySql("email = ? AND id = ?", u.email, u.id)) == 1)
  }
  
  def pwdTest {
    for (val cnt <- 1 to maxUsers) {
      val u = User.find(ByField(User.firstName, cnt.toString)).get
      assert(u.password.match_?("password"+cnt))
      assert(!u.password.match_?("dog"+cnt))
    }
  }
  
}


object User extends User with MetaMapper[User] {
  override protected def internalTableName_$ = "users"
}

class User extends ProtoUser[User] {
  def getSingleton = User
  
  def pets = Pet.findAll(ByField(Pet.owner, this.id.get))
}

class Pet extends KeyedMapper[long, Pet] {
  def getSingleton = Pet
  def primaryKeyField = id
  
  val id = new MappedLongIndex(this)
  val name = new MappedString(this) {override def maxLen = 32}
  val owner = new MappedLongForeignKey(this, User)
}

object Pet extends Pet with KeyedMetaMapper[long, Pet] {
  override protected def internalTableName_$ = "pets"
}
