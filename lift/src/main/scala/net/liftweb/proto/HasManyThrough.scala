package net.liftweb.proto

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.mapper._
import net.liftweb.util.Lazy

class HasManyThrough[From, To, Through, ThroughType](owner: Mapper[From], otherSingleton: MetaMapper[To], through: MetaMapper[Through],
    throughFromField: MappedField[ThroughType, Through] with ForeignKey[ThroughType, Through], 
    throughToField: MappedField[ThroughType, Through] with ForeignKey[ThroughType, Through]) extends LifecycleCallbacks {

  private val others = Lazy[List[To]] {
    val query = "SELECT DISTINCT "+otherSingleton.tableName_$+".* FROM "+otherSingleton.tableName_$+","+
      through.tableName_$+" WHERE "+otherSingleton.indexedField(otherSingleton).dbColumnName+" = "+
        throughToField.dbColumnName+" AND "+
        throughFromField.dbColumnName+" = ?"
          DB.prepareStatement(query) {
          st =>
          val indVal = owner.getSingleton.indexedField(owner)
          st.setObject(1, indVal.getJDBCFriendly, indVal.get.getTargetSQLType)
          DB.exec(st) {
            rs =>
              otherSingleton.createInstances(rs)
          }
        }
  }
  
   def apply(): List[To] = others.get
   
   def :=(what: Seq[MappedField[ThroughType, To]]) {}
}
