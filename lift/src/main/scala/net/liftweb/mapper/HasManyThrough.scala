package net.liftweb.mapper

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.util.{Lazy, Can, Empty, Full, Failure}
import scala.collection.mutable.HashSet

class HasManyThrough[From <: KeyedMapper[ThroughType, From],
                     To <: Mapper[To],
                     Through <: Mapper[Through],
                     ThroughType <: Any](owner: From,
				       otherSingleton: MetaMapper[To],
                                       through: MetaMapper[Through],
				       throughFromField: MappedField[ThroughType, Through], 
				       throughToField: MappedField[ThroughType, Through]) extends LifecycleCallbacks 
{
  private var theSetList: Seq[ThroughType] = Nil
  
  private val others = Lazy[List[To]] { 
    DB.use(owner.connectionIdentifier) {
      conn =>
	val query = "SELECT DISTINCT "+otherSingleton.dbTableName+".* FROM "+otherSingleton.dbTableName+","+
      through.dbTableName+" WHERE "+
      otherSingleton.dbTableName+"."+otherSingleton.indexedField(otherSingleton.asInstanceOf[To]).open_!.dbColumnName+" = "+
      through.dbTableName+"."+throughToField.dbColumnName+" AND "+
      through.dbTableName+"."+throughFromField.dbColumnName+" = ?"
      DB.prepareStatement(query, conn) {
	st =>
	  owner.getSingleton.indexedField(owner).map {
	    indVal =>
	      st.setObject(1, indVal.jdbcFriendly, indVal.targetSQLType)
	    DB.exec(st) {
	      rs =>
		otherSingleton.createInstances(owner.connectionIdentifier, rs, Empty, Empty)
	    }
	  } openOr Nil
      }
    }
  }
  
  def apply(): List[To] = others.get
  
  def get: List[To] = this()
  

  def :=(what: Seq[ThroughType]): Seq[ThroughType] = {
    theSetList = what
    theSetList
  }

  override def beforeDelete {
    through.findAll(By(throughFromField, owner.primaryKeyField)).foreach {
      toDelete => toDelete.delete_!
    }
  }
  
  override def afterUpdate {
    val current = through.findAll(By(throughFromField,owner.primaryKeyField))
    
    val newKeys = new HashSet[ThroughType];
    
    theSetList.foreach(i => newKeys += i)
    val toDelete = current.filter(c => !newKeys.contains(throughToField.actualField(c).is))
    toDelete.foreach(_.delete_!)
    
    val oldKeys = new HashSet[ThroughType];
    current.foreach(i => oldKeys += throughToField.actualField(i))

    theSetList.toList.removeDuplicates.filter(i => !oldKeys.contains(i)).foreach {
      i =>
	val toCreate = through.createInstance
      throughFromField.actualField(toCreate) := owner.primaryKeyField
      throughToField.actualField(toCreate) := i
      toCreate.save
    }

    theSetList = Nil
    others.reset
    super.afterUpdate
  }
  
  override def afterCreate {
    theSetList.toList.removeDuplicates.foreach {
      i =>
	val toCreate = through.createInstance
      throughFromField.actualField(toCreate)(owner.primaryKeyField)
      throughToField.actualField(toCreate)(i)
      toCreate.save
    }
    theSetList = Nil
    others.reset
    super.afterCreate
  }
}
