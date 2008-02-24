package net.liftweb.util

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import org.apache.commons.collections.map.{LRUMap, AbstractLinkedMap}
import org.apache.commons.collections.map.AbstractLinkedMap.LinkEntry
   

class LiftLRU[KeyType,ValueType](size: Int) extends LRUMap[KeyType,ValueType](size) {
  private[util] var beforeMove: List[LinkEntry => Any] = Nil
  private[util] var beforeRemove: List[LinkEntry => Any] = Nil
  
  protected override def moveToMRU(entry: AbstractLinkedMap.LinkEntry) {beforeMove.foreach(_(entry)); super.moveToMRU(entry)}
  protected override def removeLRU(entry: AbstractLinkedMap.LinkEntry): Boolean = {beforeRemove.foreach(_(entry)); super.removeLRU(entry)}
}

class SynchronizedLRU[KeyType,ValueType](size: Int) extends LRU[KeyType,ValueType](size, true)

class LRU[KeyType,ValueType] protected (override val underlying: java.util.Map[KeyType,ValueType])
extends scala.collection.jcl.MapWrapper[KeyType,ValueType]
{
  private var theMap: LiftLRU[KeyType,ValueType] = _
  
  def this(map: LiftLRU[KeyType,ValueType], threadSafe: Boolean) = {
    this(if (threadSafe) java.util.Collections.synchronizedMap(map) else map)
    theMap = map  
  }
  
  def this(size: Int, threadSafe: Boolean) = {
    this(new LiftLRU[KeyType,ValueType](size), threadSafe)
  }
  def this(size: Int) = this(size, false)
  
  def addBeforeMove(f: (KeyType, ValueType) => Any) {theMap.beforeMove = ((le: LinkEntry) => f(le.getKey.asInstanceOf[KeyType], le.getValue.asInstanceOf[ValueType])) :: theMap.beforeMove}
  def addBeforeRemove(f: (KeyType, ValueType) => Any) {theMap.beforeRemove = ((le: LinkEntry) => f(le.getKey.asInstanceOf[KeyType], le.getValue.asInstanceOf[ValueType])) :: theMap.beforeRemove}
}
