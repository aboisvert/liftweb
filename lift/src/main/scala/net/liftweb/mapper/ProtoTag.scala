package net.liftweb.mapper

import net.liftweb.util._
import Helpers._

trait MetaProtoTag[ModelType <: ProtoTag[ModelType], MyType <: ModelType] extends KeyedMetaMapper[Long, ModelType] { self: MyType =>
  override def dbTableName: String //  = "tags"
  def cacheSize: Int
  
  private val idCache = new LRU[Long, ModelType](cacheSize, Empty)
  private val tagCache = new LRU[String, ModelType](cacheSize, Empty)
  
  def findOrCreate(ntag: String): ModelType = synchronized {
    val tag = capify(ntag)
    if (tagCache.contains(tag)) tagCache(tag)
    else {
      find(By(name, tag)) match {
        case Full(t) => tagCache(tag) = t; t
        case _ => val ret: ModelType = (createInstance).name(tag).saveMe
        tagCache(tag) = ret
        ret
      }
    }
  }
  
  override def findDbByKey(dbId: ConnectionIdentifier,key: Long) : Can[ModelType] = synchronized {
    if (idCache.contains(key)) Full(idCache(key))
    else {
    val ret = super.findDbByKey(dbId,key)
    ret.foreach(v => idCache(key) = v)
    ret
    }
  }  
}

abstract class ProtoTag[MyType <: ProtoTag[MyType]] extends KeyedMapper[Long, MyType] with Ordered[MyType] { self: MyType =>
  // def getSingleton = Tag
  
  // the primary key for the database
  object id extends MappedLongIndex(this)
  
  def primaryKeyField = id
  
  object name extends MappedString(this, 256) {
    override def setFilter = Helpers.capify _ :: super.setFilter
    override def dbIndexed_? = true
  }
  
  def compare(other: MyType): Int = name.is.compare(other.name.is)
}

