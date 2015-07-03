package org.codeprose.api

object ApiDesignDraft {

  // Util
  // =====================================================
  //class Key(key: Symbol) { type Value }
  class Key { type Value }
  
  object Key {
    // def apply[T](key: Symbol) = new Key(key) { type Value <: T}
    def apply[T] = new Key { type Value <: T} 
  }
  trait DynamicPropertyMap {
    
    private val data = scala.collection.mutable.Map.empty[Key,Any] 
    //  get w/o get()
    def apply(key: Key) : Option[key.Value] = {
      this.get(key)
    }
    def get(key: Key) : Option[key.Value] = {
      data.get(key).asInstanceOf[Option[key.Value]]
    }
    def set(key: Key)(value: key.Value) : Unit = {
      data.update(key,value)
    }
    def remove(key: Key) : Unit = {
      data -= key
    }
    def size() : Int = { data.size }
    // Add other methods...
    
  }

  case class Token(offset: Int, text: String) extends DynamicPropertyMap 
  // Add other stuff



  // Lang
  // =====================================================

  trait DefaultLang {

    trait TokenType {}
    val tokenType = new Key{ type Value <: TokenType }
    
    // Assuming a Symbol as key
    //val tokenType = Key[TokenType]('tokenType)

  }

  // ScalaLang 
  // =====================================================
  trait ScalaLang extends DefaultLang {
    
    trait ScalaTokenType extends TokenType {
      def isComment : Boolean = true
      def bar : Boolean = true
    } 
    
    // Define set of keys    
    override val tokenType = Key[ScalaTokenType]

    // Assuming Symbol as key
    // override val tokenType = Key[ScalaTokenType]('tokenType)

    val fullName = Key[String] 
  }

  object ScalaLang extends ScalaLang

  // Use the tokens
  
  trait SomeTokenConsumer {
    
    import ScalaLang._
    val t : Token = Token(314,"return")
    
    t(tokenType).map{tt=>tt.isComment}
    println(t(fullName))
    println(t.offset)    
  }
  
}


object ApiDesignDraftUsingSymbolKeys {

  // Util
  // =====================================================
  object DynamicPropertyMap {
    abstract class Key(val key: Symbol) { type Value }
    // class Key { type Value }
    
    object Key {
      def apply[T](key: Symbol) = new Key(key) { type Value <: T}
      // def apply[T] = new Key { type Value <: T} 
    }
  }
  trait DynamicPropertyMap {
    import DynamicPropertyMap.Key 
    import scala.collection.mutable.Map
    private val data = Map.empty[Key,Any]
    
    def get(key: Key) : Option[key.Value] = { 
      data.get(key).asInstanceOf[Option[key.Value]]
    }
    
    def apply(key: Key) : Option[key.Value] = { this.get(key) }
    
    def set(key: Key)(value: key.Value) : Unit = { 
      data.update(key,value)
    }
    def remove(key: Key) : Unit = { 
      data -= key
    }
    def size() : Int = { data.size }

    
  }

  case class Token(offset: Int, text: String) extends DynamicPropertyMap 
  // Add other stuff



  // Lang
  // =====================================================

  trait DefaultLang {
    import DynamicPropertyMap._
    trait TokenType {}

    val tokenType = new Key('tokenType){ type Value <: TokenType }
    
  }

  // ScalaLang 
  // =====================================================
  trait ScalaLang extends DefaultLang {
    
    trait ScalaTokenType extends TokenType {
      def isComment : Boolean = true
      def bar : Boolean = true
    } 
    
    import DynamicPropertyMap._
    override val tokenType = Key[ScalaTokenType]('tokenType)
    val fullName = Key[String]('fullName)
  }

  object ScalaLang extends ScalaLang

  // Use the tokens
  
  trait SomeTokenConsumer {
    
    import ScalaLang._
    val t : Token = Token(314,"return")
    
    t(tokenType).map{tt=>tt.isComment}
    println(t(fullName))
    println(t.offset)    
  }
  
}
