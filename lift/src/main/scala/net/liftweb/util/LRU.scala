package net.liftweb.util

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import org.apache.commons.collections.map.{LRUMap, AbstractLinkedMap}
import org.apache.commons.collections.map.AbstractLinkedMap.LinkEntry
  
class LRU[KeyType, ValueType](size: Int) {
  private val map = new LRUMap(size)
  
  def update(k: KeyType, v: ValueType) {
    map.put(k, v)
  }

  def apply(k: KeyType): ValueType = map.get(k).asInstanceOf[ValueType]
  def contains(k: KeyType): Boolean = map.containsKey(k)
}
 
/*
class LiftLRU[KeyType,ValueType](size: Int) extends LRUMap[KeyType,ValueType](size) {
  private[util] var beforeMove: List[LinkEntry => Any] = Nil
  private[util] var beforeRemove: List[LinkEntry => Any] = Nil
  
  protected override def moveToMRU(entry: AbstractLinkedMap.LinkEntry) {beforeMove.foreach(_(entry)); super.moveToMRU(entry)}
  protected override def removeLRU(entry: AbstractLinkedMap.LinkEntry): Boolean = {beforeRemove.foreach(_(entry)); super.removeLRU(entry)}
}

// class SynchronizedLRU[+KeyType,-ValueType](size: Int) extends LRU[KeyType,ValueType](size, true)

class LRU[KeyType,ValueType] protected (override val underlying: java.util.Map[KeyType,ValueType])
extends scala.collection.jcl.MapWrapper[KeyType,ValueType]
{
  private var theMap: LiftLRU[KeyType,ValueType] = _
  
  def this(size: Int) = {
    this(new LiftLRU[KeyType,ValueType](size))
  }
  
  def addBeforeMove(f: (KeyType, ValueType) => Any) {theMap.beforeMove = ((le: LinkEntry) => f(le.getKey.asInstanceOf[KeyType], le.getValue.asInstanceOf[ValueType])) :: theMap.beforeMove}
  def addBeforeRemove(f: (KeyType, ValueType) => Any) {theMap.beforeRemove = ((le: LinkEntry) => f(le.getKey.asInstanceOf[KeyType], le.getValue.asInstanceOf[ValueType])) :: theMap.beforeRemove}
}
*/
