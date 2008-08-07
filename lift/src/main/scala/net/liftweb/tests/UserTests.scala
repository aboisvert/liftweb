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

class UserTests extends TestCase("User Tests") {
  val maxUsers = 100
  def init {
    DB.use(DefaultConnectionIdentifier) {
      conn =>

        for (cnt <- 1 to maxUsers) {
          val u = new User
          u.firstName(cnt.toString)
          u.lastName("Name "+cnt)
          u.email("mr"+cnt+"@foo.com")
          u.password("password"+cnt)
          u.save
          for (petCnt <- 1 to (1 + cnt/ 10)) {
            val p = new Pet
            p.name(""+petCnt+" of "+u.lastName)
            p.owner(u)
            p.save
          }
        }
      }
  }
  override def runTest() {


      findTest
      findAllTest
      countTest
      pwdTest
  }

  def findTest {
    assert(User.find(1).isDefined)
    assert(User.find(By(User.email, "mr9@foo.com")).isDefined)
    assert(!User.find(By(User.email, "eemr1@foo.com")).isDefined)
    assert(User.find(BySql("email = ?", "mr9@foo.com")).isDefined)
    assert(!User.find(BySql("email = ?", "eemr1@foo.com")).isDefined)
    assert(User.find(BySql("email = ?", "mr9@foo.com"), BySql("firstname = ?", "9"), BySql("firstname = ?", 9)).isDefined)
    assert(User.find(BySql("email = ? AND firstname = ?", "mr9@foo.com", "9")).isDefined)
    assert(!User.find(BySql[User]("email = ? AND firstname = ?", "mr1@foo.com", "33")).isDefined)
    val u = User.find(33).open_!
    assert(User.find(BySql("email = ?", u.email)).open_!.id == u.id)
    assert(User.find(BySql("id = ?", 33)).open_!.id == u.id)
    assert(User.find(BySql("id = ?", u.id)).open_!.id == u.id)

    for (uKey <- 1 to maxUsers) {
      val u = User.find(uKey)
      assert(u.isDefined)
      val user = u.open_!
      val pl = user.pets.length
      assert(pl == (1 + uKey / 10), "Wanted "+pl+" got "+(1 + uKey / 10))
    }
  }

  def findAllTest {
    assert(User.findAll.length == maxUsers, "Actual len "+User.findAll.length+" expected "+maxUsers)
    assert(User.findAll(MaxRows(25), StartAt(25)).length == 25)
    assert(User.findAll(StartAt[User](25)).length == (maxUsers - 25))
    assert(User.findAll(By(User.email, "mr33@foo.com")).length == 1)
    assert(User.findAll(By(User.email, "dogmr33@foo.com")).length == 0)
    assert(User.findAll(BySql[User]("email = ?", "mr9@foo.com")).length == 1)
    assert(User.findAll(BySql[User]("email = ?", "eemr1@foo.com")).length == 0)
    assert(User.findAll(BySql("email = ?", "mr9@foo.com"), BySql("firstname = ?", "9")).length == 1)
    assert(User.findAll(BySql[User]("email = ? AND firstname = ?", "mr9@foo.com", "9")).length == 1)
    assert(User.findAll(BySql[User]("email = ? AND firstname = ?", "mr1@foo.com", "33")).length == 0)
    val u = User.find(33).open_!
    assert(User.findAll(BySql[User]("email = ?", u.email)).length == 1)
    assert(User.findAll(OrderBy(User.firstName, Ascending))(0).firstName == "1")
    assert(User.findAll(OrderBy(User.firstName, Descending))(0).firstName == "99")
  }

  def countTest {
    assert(User.count == maxUsers)

    assert(User.count(By(User.email, "mr33@foo.com")) == 1)
    assert(User.count(By(User.email, "dogmr33@foo.com")) == 0)
    assert(User.count(BySql("email = ?", "mr9@foo.com")) == 1)
    assert(User.count(BySql("email = ?", "eemr1@foo.com")) == 0)
    assert(User.count(BySql("email = ?", "mr9@foo.com"), BySql("firstname = ?", "9")) == 1)
    assert(User.count(BySql("email = ? AND firstname = ?", "mr9@foo.com", "9")) == 1)
    assert(User.count(BySql("email = ? AND firstname = ?", "mr1@foo.com", "33")) == 0)
    assert(User.count(BySql("email = ? AND firstname = ?", "mr1@foo.com", "1")) == 1)
    val u = User.find(33).open_!
    assert(User.count(BySql("email = ?", u.email)) == 1)
    assert(User.count(BySql("email = ? AND id = ?", u.email, u.id)) == 1)
  }

  def pwdTest {
    for (cnt <- 1 to maxUsers) {
      val u = User.find(By(User.firstName, cnt.toString)).open_!
      assert(u.password.match_?("password"+cnt))
      assert(!u.password.match_?("dog"+cnt))
    }
  }

}


object User extends User with KeyedMetaMapper[long, User] {
  override def dbTableName = "users"

}

class User extends ProtoUser[User] {
  def getSingleton = User

  def pets = Pet.findAll(By(Pet.owner, this.id))
}

class Pet extends KeyedMapper[Long, Pet] {
  def getSingleton = Pet
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object name extends MappedString(this, 32)
  object owner extends MappedLongForeignKey(this, User)
  object icon extends MappedBinary(this)
}

object Pet extends Pet with KeyedMetaMapper[long, Pet] {
  override def dbTableName = "pets"
}
