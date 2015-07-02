package org.codeprose.api

object TokenPropertyMap {
  abstract class Key(val name: Symbol){
    type Value
  }  
}

class TokenPropertyMap {
  import TokenPropertyMap.Key
  import collection.mutable.Map
  
  private val data = Map.empty[Key,Any]
  
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
    data.map(e=>e._1.name + " -> " + e._2).mkString(", ")
  }
  
}

/*
 * Key traits
 */
object KeyTraits {
  import org.codeprose.api.TokenPropertyMap.Key 
  trait IntValued extends Key {
    type Value = Int
  }
  trait StringValued extends Key {
    type Value = String
  }
  
  trait TextValued extends Key {
    type Value = String
  }
  
  trait SourcePositionValued extends Key {
    import org.codeprose.api.TokenProperties.SourcePosition
    type Value = SourcePosition
  }  

  trait TokenTypeValued extends Key {
    import org.codeprose.api.TokenType
    type Value = TokenType
  }
  
  trait TokenValued extends Key {
    import org.codeprose.api.ScalaLang.ScalaToken
    type Value = ScalaToken
  }
  
}

/*
 * TokenPropertyMap Keys
 */
trait Keys {
  import org.codeprose.api.TokenPropertyMap.Key
  import org.codeprose.api.KeyTraits._
  val offset = new Key('offset) with IntValued
  val text = new Key('text) with TextValued  
}

/*
 * Scala language keys.
 */
object ScalaLangKeys extends org.codeprose.api.Keys {
   import org.codeprose.api.KeyTraits._
   import org.codeprose.api.TokenPropertyMap.Key
   
   val tokenType = new Key('tokenType) with TokenTypeValued
   val token = new Key('token) with TokenValued
   val declaredAt = new Key('declaredAt) with SourcePositionValued
   val typeId = new Key('typeId) with IntValued   
   // TODO be extended
}
  
  
