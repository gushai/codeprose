package org.codeprose.api

import scala.collection.mutable.ArrayBuffer
import org.codeprose.util.DynamicPropertyMap
import org.codeprose.api.TokenProperties.SourcePosition

// Token
class Token(val offset: Int, val text: String) extends DynamicPropertyMap {
	val length = text.length
			val range = Range(offset,offset+length)  
}


// DefaultLang

trait DefaultLang {
	import DynamicPropertyMap._
	trait TokenType {}
  
  // Keys
	val tokenType = new Key('tokenType){ type Value <: TokenType }

}

// ScalaLang 

trait ScalaLang extends DefaultLang {

  case class ScalaTokenType(val name: String, isXml: Boolean = false) extends TokenType {
  
  	import org.codeprose.api.ScalaTokens._
  	def isNewline = this == ScalaTokens.NEWLINE || this == ScalaTokens.NEWLINES
  //  def isKeyword = ScalaTokens.KEYWORDS contains this
  //	def isComment = ScalaTokens.COMMENTS contains this
  //	def isId = ScalaTokens.IDS contains this
  //	def isLiteral = ScalaTokens.LITERALS contains this
  
  	override lazy val toString = name
  
  }
  
  // ScalaLang Keys  
  import DynamicPropertyMap._
  
  val declaredAt = new Key('declaredAt) { type Value = SourcePosition } 
  val fullName = new Key('fullName) { type Value = String }  
  val isArrowTye = new Key('isArrowType) { type Value = Boolean }
  override val tokenType = new Key('tokenType) { type Value = ScalaTokenType }
  

}

object ScalaLang extends ScalaLang
