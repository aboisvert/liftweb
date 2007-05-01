package com.skittr.model

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._
import com.skittr.actor._
import net.liftweb.util.Helpers._

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with KeyedMetaMapper[long, User] {
  override def dbTableName = "users" // define the DB table name
  
  // define the order fields will appear in forms and output
  override def fieldOrder = id :: name :: firstName :: lastName :: email ::  password :: Nil
  
  override def afterCreate = &UserList.startUser :: super.afterCreate
  
  
  /**
    * Calculate a random persiod of at least 2 minutes and at most 8 minutes
    */
  // def randomPeriod: long = 2.minutes + randomLong(6.minutes)
  def randomPeriod: long = 15.seconds + randomLong(45.seconds)
    
  def shouldAutogen_? = true
      
  // the number of test users to create
  def createdCount = 100
  
  def createTestUsers {
    (1 to createdCount).foreach {
      i =>
      
      User.create.firstName("Mr.").lastName("User "+i).email("user"+i+"@skittr.com").
        password("password"+i).name("test"+i).dontStart.saveMe
    }
    
    (1 to createdCount * 7).foreach {
      i =>
      val owner = randomLong(createdCount) + 1
      val friend = randomLong(createdCount) + 1
      if (owner != friend && Friend.count(By(Friend.friend, friend), By(Friend.owner, owner)) == 0) {
        Friend.create.owner(owner).friend(friend).save
      }
    }
    
  }
  
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends ProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  def primaryKeyField = id
  
  def wholeName = firstName+" "+lastName
  private var startMeUp = true
  
  // The Name of the User
  val name =  new MappedString(this) {
    override def setFilter = &notNull :: &toLower :: &trim :: super.setFilter
    
    override def validations = &valMinLen(3, "Name too short") :: 
     &valUnique("The name '"+get+"' is already taken") :: 
     super.validations
     
    override def dbIndexed_? = true
  }
  
  def dontStart = {
    startMeUp = false
    this
  }
  
  def shouldStart_? = startMeUp
}
