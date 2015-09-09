package org.codeprose.util


/**
 * Contains the key used in the DynamicPropertyMap. 
 */
object DynamicPropertyMap {
  
  /**
   * Key used in the DynamicPropertyMap.
   * 
   * The type member Value is used to guarantee the return type of getters in the 
   * DynamicPropertyMap.
   * 
   * @param key Name of the key.     
   */
  abstract class Key(val key: Symbol){ type Value }
  
  /**
   * Shorthand constructor for Key.
   */
  object Key {
    def apply[T](key: Symbol) = new Key(key) { type Value <: T}
  }
}

/**
 * Dynamic property map with key dependent return types.
 * 
 * Data is stored in a mutable map which is accessed via keys of
 * type DynamicPropertyMap.Key. The key's 
 * type argument Key.Value guarantees type safety in the return type.
 * 
 * Used as basis for 
 *  - org.codeprose.api.Token
 *  - org.codeprose.api.ProjectSummary 
 *
 */
trait DynamicPropertyMap {
  import DynamicPropertyMap.Key
  import collection.mutable.Map
  
  private val data = Map.empty[Key,Any]
  
  /**
   * Optionally returns the value associated with a key.
   * Short hand for get()
   * @param key Key
   * @return    Option[key.Value] 
   */
  def apply(key: Key) : Option[key.Value] = {
    data.get(key).asInstanceOf[Option[key.Value]]
  }
  
  /**
   * Optionally returns the value associated with a key.
   * @param key Key.
   * @return    Option[key.Value].
   */
  def get(key: Key) : Option[key.Value] = {
    data.get(key).asInstanceOf[Option[key.Value]]
  }
  
  /**
   * Adds a new key/value pair to this map.
   * @param key Key.
   */
  def set(key: Key)(value: key.Value) : Unit = {
    data.update(key,value)
  }
  
  /**
   * Removes key from this map.
   * @param key Key.
   */
  def remove(key: Key) : Unit = {
    data-=key
  }
  
  /**
   * Returns the number of elements in this map.
   * @return  Number of elements.
   */
  def size() : Int = {
    data.size
  }
  
  /**
   * Pretty prints the elements in this map.
   * @return  String containing the mapping key->value for each element.
   */
  override def toString() : String = {
    data.map(e=>e._1.key + " -> " + e._2).mkString("; ")
  }
  
}
