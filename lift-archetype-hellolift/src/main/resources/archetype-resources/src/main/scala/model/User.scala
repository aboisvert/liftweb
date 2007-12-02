package ${groupId}.model

import net.liftweb.mapper._
import net.liftweb.util._

/**
 * The singleton that has methods for accessing the database. LiftNote: 2
 */
object User extends User with MetaMegaProtoUser[User, User with KeyedMetaMapper[Long, User]] {
  override val dbTableName = "users"
  override def screenWrap = Full(<lift:surround with="default" at="content">
			       <lift:bind /></lift:surround>) // LiftNote: 6
  override def signupFields = firstName :: lastName :: email :: locale :: timezone :: password :: blogtitle :: Nil
  override val skipEmailValidation = true // LiftNote: 4
}

/**
 * An O-R mapped "User" class that includes first name, last name, password. LiftNote: 1
 */
class User extends MegaProtoUser[User] {
  
  def getSingleton = User // what's the "meta" server
  def primaryKeyField = id
  object blogtitle extends MappedString(this, 128)
}

