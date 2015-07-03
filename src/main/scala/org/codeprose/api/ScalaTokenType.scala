package org.codeprose.api





trait TokenType {
	val name : String 
}

case class ScalaTokenType(val name: String, isXml: Boolean = false) extends TokenType {

	import org.codeprose.api.{ScalaTokens => Tokens}

	def isNewline = this == Tokens.NEWLINE || this == Tokens.NEWLINES

			def isKeyword = Tokens.KEYWORDS contains this

			def isComment = Tokens.COMMENTS contains this

			def isId = Tokens.IDS contains this

			def isLiteral = Tokens.LITERALS contains this

			override lazy val toString = name

}


trait K {
	type Value
}

trait Lang {
	trait TT
	val tt = new K {type Value <: TT}
}

trait ScalaL extends Lang {
	trait STT extends TT { def isComment: Boolean = true}
	override val tt = new K {type Value = STT}

	// TODO: Add Scalalang Keys


}


trait DefaultToken {???}

trait EnrichedToken {
	val token: DefaultToken = ???
			val tpm : TokenPropertyMap = ???
}

trait Consumer {
	// Ops defined here avail on all EnrichedTokens
	implicit class EnrichedTokenOps(t: EnrichedToken) {
		def tokenType(implicit l: Lang): Option[l.tt.Value] = ??? 
				// t.tpm.get()
	}

	//  Not needed 
	//  implicit class DefaultTokenOps(t: EnrichedToken) {
	//    def tokenType(implicit l: Lang): Option[l.tt.Value] = ??? 
	//      // t.tpm.get()
	//  }

}

object MyScalaLConsumer extends Consumer {

	implicit object ScalaL extends ScalaL

	val et : EnrichedToken = ???
			et.tokenType.map{tt => tt.isComment}

}


// =====================
// alt design

object AltDesingFancy {
	// Util
  class Key(key: Symbol) { type Value }
	object Key {
		 def apply[T](key: Symbol) = new Key(key) { type Value <: T}
		//def apply[T] = new Key { type Value <: T} 
	}
  
	trait DynamicPropertyMap {
    
    private val data = scala.collection.mutable.Map.empty[Symbol,Any] 
		//  get w/o get()
		def apply(key: Key) : Option[key.Value] = { ??? }
//    def get(key: Key) : Option[key.Value] = {
//      data.get(key).asInstanceOf[Option[key.Value]]
//    }
    
				// Add other methods...
    
	}

  case class Token(offset: Int, text: String) extends DynamicPropertyMap 
  // Add other stuff



// Lang

  trait DefaultLang {

	  trait TokenType {}

    val tokenType = Key[TokenType]('tokenType)
//    val tokenType = new Key{ type Value <: TokenType }
  }

  trait ScalaLang extends DefaultLang {
    
    trait ScalaTokenType extends TokenType {
      def isComment : Boolean = ???
    } 
    
    // Define set of keys
    override val tokenType = Key[ScalaTokenType]('tokenType)
    
    // Other keys
  }

  object ScalaLang extends ScalaLang
  
  trait Consumer {
    import ScalaLang._
    val t : Token = ???
    
    t(tokenType).map{tt=>tt.isComment}
    println(t.offset)    
  }
  
}
