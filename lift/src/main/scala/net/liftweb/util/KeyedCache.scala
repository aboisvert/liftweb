package net.liftweb.util

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

/**
 * A simple Read-through cache.
 *
 * An example of using it with a ProtoUser subclass:
 *
 * object UserCache extends KeyedCache[Long, User](100, Full(0.75f), 
 *   (id: Long) => User.find(By(User.id, id)))
 *
 * @param size the size of the cache
 * @param loadFactor the optional Load Factor
 * @param cons A function that will take a value of type K and return a Can[T] 
 *   populated into the cache if the return value is Full.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class KeyedCache[K, T](size: Int, loadFactor: Can[Float], cons: K => Can[T]) {
  val cache = new LRU[K, T](size, loadFactor)

  def apply(key: K): Can[T] = if (cache.contains(key)) {
    Full(cache(key))
  } else {
    cons(key) match {
      case f@Full(v) => cache.update(key, v); f
      case Empty => Empty
    }
  }
}

