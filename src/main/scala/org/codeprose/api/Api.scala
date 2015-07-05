package org.codeprose.api

import scala.collection.mutable.ArrayBuffer
import org.codeprose.util.DynamicPropertyMap
import org.codeprose.api.TokenProperties.SourcePosition

// Token
class Token(val offset: Int, val text: String) extends DynamicPropertyMap {
	val length = text.length
  val range = Range(offset,offset+length)
  def toPrettyString() : String = {
   s"""Token($text,$offset) w/ prop: """ + toString()   
  }
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
    
    // TODO: Fix: Compiler error: type missmatch
//[error]  found   : ScalaLang.this.ScalaTokenType
//[error]  required: org.codeprose.api.ScalaLang.ScalaTokenType
//[error]     def isKeyword = ScalaTokens.KEYWORDS contains this
  //  def isKeyword = ScalaTokens.KEYWORDS contains this 
  //	def isComment = ScalaTokens.COMMENTS contains this
  //	def isId = ScalaTokens.IDS contains this
  //	def isLiteral = ScalaTokens.LITERALS contains this
  def isKeyword = (this == ABSTRACT || this == CASE || this == CATCH || this == CLASS || this == DEF || 
        this == DO || this == ELSE || this == EXTENDS || this == FINAL || 
        this == FINALLY || this == FOR || this == FORSOME || this == IF || this == IMPLICIT || 
        this == IMPORT || this == LAZY || this == MATCH || this == NEW || 
        this == OBJECT || this == OVERRIDE || this == PACKAGE || this == PRIVATE || this == PROTECTED || 
        this == RETURN || this == SEALED || this == SUPER || this == THIS || 
        this == THROW || this == TRAIT || this == TRY || this == TYPE || 
        this == VAL || this == VAR || this == WHILE || this == WITH || this == YIELD)
  def isComment = (this == ScalaTokens.MULTILINE_COMMENT || this == ScalaTokens.LINE_COMMENT || this == ScalaTokens.XML_COMMENT)        
  def isId = (this == VARID || this == PLUS || this == MINUS || this == STAR || this == PIPE || this == TILDE || this == EXCLAMATION)
  def isLiteral = (this == CHARACTER_LITERAL || this == INTEGER_LITERAL || this == FLOATING_POINT_LITERAL || this == STRING_LITERAL || this == STRING_PART || this == SYMBOL_LITERAL || this == TRUE || this == FALSE || this == NULL)    
  
    
  	override lazy val toString = name
  
  }
  
  // ScalaLang Keys  
  import DynamicPropertyMap._
  
  val declaredAs = new Key('declaredAs) { type Value = String }
  val declaredAt = new Key('declaredAt) { type Value = SourcePosition }
  
  val fullName = new Key('fullName) { type Value = String }  
  val isArrowType = new Key('isArrowType) { type Value = Boolean }
  override val tokenType = new Key('tokenType) { type Value = ScalaTokenType }
  val typeId = new Key('typeId) { type Value = Int }
  

}

object ScalaLang extends ScalaLang
