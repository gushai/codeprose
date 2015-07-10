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

// Container of information exchange
object Api {
  type TokenInfoContainer = scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[Token])]
  type MetaInfoContainer = scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[String])]
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
  
  	import org.codeprose.api.ScalaLang.Tokens._
  	def isNewline = this == Tokens.NEWLINE || this == Tokens.NEWLINES
    
    // TODO: Fix: Compiler error: type missmatch
//[error]  found   : ScalaLang.this.ScalaTokenType
//[error]  required: org.codeprose.api.ScalaLang.ScalaTokenType
//[error]     def isKeyword = ScalaTokens.KEYWORDS contains this
    def isKeyword = Tokens.KEYWORDS contains this 
  	def isComment = Tokens.COMMENTS contains this
  	def isId = Tokens.IDS contains this
  	def isLiteral = Tokens.LITERALS contains this
    
//  def isKeyword = (this == ABSTRACT || this == CASE || this == CATCH || this == CLASS || this == DEF || 
//        this == DO || this == ELSE || this == EXTENDS || this == FINAL || 
//        this == FINALLY || this == FOR || this == FORSOME || this == IF || this == IMPLICIT || 
//        this == IMPORT || this == LAZY || this == MATCH || this == NEW || 
//        this == OBJECT || this == OVERRIDE || this == PACKAGE || this == PRIVATE || this == PROTECTED || 
//        this == RETURN || this == SEALED || this == SUPER || this == THIS || 
//        this == THROW || this == TRAIT || this == TRY || this == TYPE || 
//        this == VAL || this == VAR || this == WHILE || this == WITH || this == YIELD)
  //def isComment = (this == MULTILINE_COMMENT || this == LINE_COMMENT || this == XML_COMMENT)        
  //def isId = (this == VARID || this == PLUS || this == MINUS || this == STAR || this == PIPE || this == TILDE || this == EXCLAMATION)
  //def isLiteral = (this == CHARACTER_LITERAL || this == INTEGER_LITERAL || this == FLOATING_POINT_LITERAL || this == STRING_LITERAL || this == STRING_PART || this == SYMBOL_LITERAL || this == TRUE || this == FALSE || this == NULL)    
  
    
  	override lazy val toString = name
  
  }
  
  // ScalaTokens
  // ============================================================================
  /* Based on
   * scalariform/scalariform/src/main/scala/com/danieltrinh/scalariform/lexer/Tokens.scala
   */
object Tokens {
     
  val PACKAGE = ScalaTokenType("PACKAGE")
  val STAR = ScalaTokenType("STAR")
  val WHILE = ScalaTokenType("WHILE")
  val CASE = ScalaTokenType("CASE")
  val NEW = ScalaTokenType("NEW")
  val DO = ScalaTokenType("DO")
  val EQUALS = ScalaTokenType("EQUALS")
  val SUBTYPE = ScalaTokenType("SUBTYPE")
  val EOF = ScalaTokenType("EOF")
  val SEALED = ScalaTokenType("SEALED")
  val TYPE = ScalaTokenType("TYPE")
  val LBRACKET = ScalaTokenType("LBRACKET")
  val FINAL = ScalaTokenType("FINAL")
  val RPAREN = ScalaTokenType("RPAREN")
  val IMPORT = ScalaTokenType("IMPORT")
  val STRING_LITERAL = ScalaTokenType("STRING_LITERAL")
  val STRING_PART = ScalaTokenType("STRING_PART")
  val FLOATING_POINT_LITERAL = ScalaTokenType("FLOATING_POINT_LITERAL")
  val EXCLAMATION = ScalaTokenType("EXCLAMATION")
  val NEWLINES = ScalaTokenType("NEWLINES")
  val THIS = ScalaTokenType("THIS")
  val RETURN = ScalaTokenType("RETURN")
  val VAL = ScalaTokenType("VAL")
  val VAR = ScalaTokenType("VAR")
  val SUPER = ScalaTokenType("SUPER")
  val RBRACE = ScalaTokenType("RBRACE")
  val LINE_COMMENT = ScalaTokenType("LINE_COMMENT")
  val PRIVATE = ScalaTokenType("PRIVATE")
  val NULL = ScalaTokenType("NULL")
  val ELSE = ScalaTokenType("ELSE")
  val CHARACTER_LITERAL = ScalaTokenType("CHARACTER_LITERAL")
  val MATCH = ScalaTokenType("MATCH")
  val TRY = ScalaTokenType("TRY")
  val WS = ScalaTokenType("WS")
  val SUPERTYPE = ScalaTokenType("SUPERTYPE")
  val INTEGER_LITERAL = ScalaTokenType("INTEGER_LITERAL")
  val OP = ScalaTokenType("OP")
  val USCORE = ScalaTokenType("USCORE")
  val LOWER = ScalaTokenType("LOWER")
  val CATCH = ScalaTokenType("CATCH")
  val FALSE = ScalaTokenType("FALSE")
  val VARID = ScalaTokenType("VARID")
  val THROW = ScalaTokenType("THROW")
  val UPPER = ScalaTokenType("UPPER")
  val PROTECTED = ScalaTokenType("PROTECTED")
  val CLASS = ScalaTokenType("CLASS")
  val DEF = ScalaTokenType("DEF")
  val LBRACE = ScalaTokenType("LBRACE")
  val FOR = ScalaTokenType("FOR")
  val LARROW = ScalaTokenType("LARROW")
  val RARROW = ScalaTokenType("RARROW")
  val ABSTRACT = ScalaTokenType("ABSTRACT")
  val LPAREN = ScalaTokenType("LPAREN")
  val IF = ScalaTokenType("IF")
  val AT = ScalaTokenType("AT")
  val MULTILINE_COMMENT = ScalaTokenType("MULTILINE_COMMENT")
  val SYMBOL_LITERAL = ScalaTokenType("SYMBOL_LITERAL")
  val OBJECT = ScalaTokenType("OBJECT")
  val COMMA = ScalaTokenType("COMMA")
  val YIELD = ScalaTokenType("YIELD")
  val TILDE = ScalaTokenType("TILDE")
  val PLUS = ScalaTokenType("PLUS")
  val PIPE = ScalaTokenType("PIPE")
  val VIEWBOUND = ScalaTokenType("VIEWBOUND")
  val RBRACKET = ScalaTokenType("RBRACKET")
  val DOT = ScalaTokenType("DOT")
  val WITH = ScalaTokenType("WITH")
  val IMPLICIT = ScalaTokenType("IMPLICIT")
  val LAZY = ScalaTokenType("LAZY")
  val TRAIT = ScalaTokenType("TRAIT")
  val HASH = ScalaTokenType("HASH")
  val FORSOME = ScalaTokenType("FORSOME")
  val MINUS = ScalaTokenType("MINUS")
  val TRUE = ScalaTokenType("TRUE")
  val SEMI = ScalaTokenType("SEMI")
  val COLON = ScalaTokenType("COLON")
  val OTHERID = ScalaTokenType("OTHERID")
  val NEWLINE = ScalaTokenType("NEWLINE")
  val FINALLY = ScalaTokenType("FINALLY")
  val OVERRIDE = ScalaTokenType("OVERRIDE")
  val ARROW = ScalaTokenType("ARROW")
  val EXTENDS = ScalaTokenType("EXTENDS")
  val INTERPOLATION_ID = ScalaTokenType("INTERPOLATION_ID")
  val XML_START_OPEN = ScalaTokenType("XML_START_OPEN", isXml = true)
  val XML_EMPTY_CLOSE = ScalaTokenType("XML_EMPTY_CLOSE", isXml = true)
  val XML_TAG_CLOSE = ScalaTokenType("XML_TAG_CLOSE", isXml = true)
  val XML_END_OPEN = ScalaTokenType("XML_END_OPEN", isXml = true)
  val XML_WHITESPACE = ScalaTokenType("XML_WHITESPACE", isXml = true)
  val XML_ATTR_EQ = ScalaTokenType("XML_ATTR_EQ", isXml = true)
  val XML_ATTR_VALUE = ScalaTokenType("XML_ATTR_VALUE", isXml = true)
  val XML_NAME = ScalaTokenType("XML_NAME", isXml = true)
  val XML_PCDATA = ScalaTokenType("XML_PCDATA", isXml = true)
  val XML_COMMENT = ScalaTokenType("XML_COMMENT", isXml = true)
  val XML_CDATA = ScalaTokenType("XML_CDATA", isXml = true)
  val XML_UNPARSED = ScalaTokenType("XML_UNPARSED", isXml = true)
  val XML_PROCESSING_INSTRUCTION = ScalaTokenType("XML_PROCESSING_INSTRUCTION", isXml = true)

  val KEYWORDS = Set(
    ABSTRACT, CASE, CATCH, CLASS, DEF,
    DO, ELSE, EXTENDS, FINAL,
    FINALLY, FOR, FORSOME, IF, IMPLICIT,
    IMPORT, LAZY, MATCH, NEW,
    OBJECT, OVERRIDE, PACKAGE, PRIVATE, PROTECTED,
    RETURN, SEALED, SUPER, THIS,
    THROW, TRAIT, TRY, TYPE,
    VAL, VAR, WHILE, WITH, YIELD
  )

  val COMMENTS = Set(LINE_COMMENT, MULTILINE_COMMENT, XML_COMMENT)

  val IDS = Set(VARID, PLUS, MINUS, STAR, PIPE, TILDE, EXCLAMATION)

  val LITERALS = Set(CHARACTER_LITERAL, INTEGER_LITERAL, FLOATING_POINT_LITERAL, STRING_LITERAL, STRING_PART, SYMBOL_LITERAL, TRUE, FALSE, NULL)

}
  
  
  
  // ScalaLang Keys
  // ============================================================================
  import DynamicPropertyMap._
  
  val declaredAs = new Key('declaredAs) { type Value = String }
  val declaredAt = new Key('declaredAt) { type Value = SourcePosition }
  
  val fullName = new Key('fullName) { type Value = String }  
  val isArrowType = new Key('isArrowType) { type Value = Boolean }
  override val tokenType = new Key('tokenType) { type Value = ScalaTokenType }
  val typeId = new Key('typeId) { type Value = Int }
  
  
  
  

}

object ScalaLang extends ScalaLang
