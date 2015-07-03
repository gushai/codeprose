package org.codeprose.util

object DynamicPropertyMap {
  abstract class Key(val key: Symbol){ type Value }
  object Key {
    def apply[T](key: Symbol) = new Key(key) { type Value <: T}
  }
}

trait DynamicPropertyMap {
  import DynamicPropertyMap.Key
  import collection.mutable.Map
  
  private val data = Map.empty[Key,Any]
  
  def apply(key: Key) : Option[key.Value] = {
    data.get(key).asInstanceOf[Option[key.Value]]
  }
  
  def get(key: Key) : Option[key.Value] = {
    data.get(key).asInstanceOf[Option[key.Value]]
  }
  
  def set(key: Key)(value: key.Value) : Unit = {
    data.update(key,value)
  }
  
  def remove(key: Key) : Unit = {
    data-=key
  }
  
  def size() : Int = {
    data.size
  }
  
  override def toString() : String = {
    data.map(e=>e._1.key + " -> " + e._2).mkString(", ")
  }
  
}
