package net.liftweb.proto

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.mapper._
import net.liftweb.util.Lazy

class HasManyThrough[ From<:(Mapper[From] with Keyed[ThroughType, From]), To<:Mapper[To], Through<:Mapper[Through], ThroughType<:Any](owner: From,
    otherSingleton: MetaMapper[To], through: MetaMapper[Through],
    throughFromField: MappedField[ThroughType, Through], 
    throughToField: MappedField[ThroughType, Through]) extends LifecycleCallbacks {

  private var theSetList: Seq[ThroughType] = Nil
  
  private val others = Lazy[List[To]] {
    val query = "SELECT DISTINCT "+otherSingleton.tableName_$+".* FROM "+otherSingleton.tableName_$+","+
      through.tableName_$+" WHERE "+otherSingleton.indexedField(otherSingleton.asInstanceOf[To]).get.dbColumnName+" = "+
        throughToField.dbColumnName+" AND "+
        throughFromField.dbColumnName+" = ?"
          DB.prepareStatement(query) {
          st =>
          owner.getSingleton.indexedField(owner).map {
          indVal =>
          st.setObject(1, indVal.getJDBCFriendly, indVal.getTargetSQLType)
          DB.exec(st) {
            rs =>
              otherSingleton.createInstances(rs)
          }
          } getOrElse Nil
        }
  }
  
   def apply(): List[To] = others.get
   
   def :=(what: Seq[ThroughType]):Seq[ThroughType] = {theSetList = what; theSetList}
   
   def +=(what: ThroughType): Seq[ThroughType] = {
     theSetList = what :: theSetList.toList
     theSetList
   }

   def -=(what: ThroughType): Seq[ThroughType] = {
     theSetList = theSetList.toList.remove{n => n == what}
     theSetList
   }

   override def beforeDelete {
     through.findAll(ByField[Through, ThroughType](throughFromField, owner.primaryKeyField)).foreach {
       toDelete => toDelete.delete_!
     }
   }
   
   override def afterCreate {
     theSetList.toList.removeDuplicates.foreach {
       i =>
       val toCreate = through.createInstance
       throughFromField.getActualField(toCreate) := owner.primaryKeyField
       throughToField.getActualField(toCreate) := i
       toCreate.save
     }
     theSetList = Nil
     super.afterCreate
   }
}
