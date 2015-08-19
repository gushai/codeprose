package org.codeprose.api

import scala.collection.mutable.ArrayBuffer
import org.codeprose.util.DynamicPropertyMap

// Token
class Token(val offset: Int, val text: String) extends DynamicPropertyMap {
	val length = text.length
  val range = Range(offset,offset+length)
  def toPrettyString() : String = { s"""Token($text,$offset,$length) w/ prop: """ + toString() }
}

class ProjectSummary() extends DynamicPropertyMap


class ProjectInfo (  
  val enrichedTokens : scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[Token])],
  val summary : ProjectSummary
  ) {
}


// Container of information exchange
object Api {
  type EnrichedTokenContainer = scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[Token])]
}



// Container for project specific information


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
    
    def isKeyword = Tokens.KEYWORDS contains this 
  	def isComment = Tokens.COMMENTS contains this
  	def isId = Tokens.IDS contains this
  	def isLiteral = Tokens.LITERALS contains this
    
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
  
  

object SourceSymbol {
  
  sealed trait SourceSymbol

  case object ObjectSymbol extends SourceSymbol
  case object ClassSymbol extends SourceSymbol
  case object TraitSymbol extends SourceSymbol
  case object PackageSymbol extends SourceSymbol
  case object ConstructorSymbol extends SourceSymbol
  case object ImportedNameSymbol extends SourceSymbol
  case object TypeParamSymbol extends SourceSymbol
  case object ParamSymbol extends SourceSymbol
  case object VarFieldSymbol extends SourceSymbol
  case object ValFieldSymbol extends SourceSymbol
  case object OperatorFieldSymbol extends SourceSymbol
  case object VarSymbol extends SourceSymbol
  case object ValSymbol extends SourceSymbol
  case object FunctionCallSymbol extends SourceSymbol
  case object ImplicitConversionSymbol extends SourceSymbol
  case object ImplicitParamsSymbol extends SourceSymbol
  case object DeprecatedSymbol extends SourceSymbol
  
  val allSymbols: List[SourceSymbol] = List(
    ObjectSymbol, ClassSymbol, TraitSymbol, PackageSymbol, ConstructorSymbol, ImportedNameSymbol, TypeParamSymbol,
    ParamSymbol, VarFieldSymbol, ValFieldSymbol, OperatorFieldSymbol, VarSymbol, ValSymbol, FunctionCallSymbol,
    ImplicitConversionSymbol, ImplicitParamsSymbol, DeprecatedSymbol
  )
  
  def mapEnsimeToCodeprose(sourceSym : org.ensime.api.SourceSymbol) : SourceSymbol = {
    sourceSym match {
      case org.ensime.api.ObjectSymbol => { return ObjectSymbol } 
      case org.ensime.api.ClassSymbol => { return ClassSymbol } 
      case org.ensime.api.TraitSymbol => { return TraitSymbol } 
      case org.ensime.api.PackageSymbol => { return PackageSymbol } 
      case org.ensime.api.ConstructorSymbol => { return ConstructorSymbol } 
      case org.ensime.api.ImportedNameSymbol => { return ImportedNameSymbol } 
      case org.ensime.api.TypeParamSymbol => { return TypeParamSymbol } 
      case org.ensime.api.ParamSymbol => { return ParamSymbol } 
      case org.ensime.api.VarFieldSymbol => { return VarFieldSymbol } 
      case org.ensime.api.ValFieldSymbol => { return ValFieldSymbol } 
      case org.ensime.api.OperatorFieldSymbol => { return OperatorFieldSymbol } 
      case org.ensime.api.VarSymbol => { return VarSymbol } 
      case org.ensime.api.ValSymbol => { return ValSymbol } 
      case org.ensime.api.FunctionCallSymbol => { return FunctionCallSymbol } 
      case org.ensime.api.ImplicitConversionSymbol => { return ImplicitConversionSymbol } 
      case org.ensime.api.ImplicitParamsSymbol => { return ImplicitParamsSymbol } 
      case org.ensime.api.DeprecatedSymbol => { return DeprecatedSymbol }
      //case _ => { new Exception("Unknown SourceSymbol!") }
    }
    
  }
}



  
  // ScalaLang Keys
  // ============================================================================
  
    
  // TODO: Is a grouing of keys possible? 
    
    // Key for Tokens
    // ==========================================================================
        
        import DynamicPropertyMap._

        val internalTokenId = new Key('internalTokenId){ type Value = Int }
        
        val declaredAs = new Key('declaredAs) { type Value = String }
        
        val declaredAt = new Key('declaredAt) { type Value = OffsetSourcePositionWithTokenId }
        
        //val declaredAt_TokenIdSrcPos = new Key('declaredAt_TokenIdSrcPos){ type Value = SourcePositionWithTokenId }
        
        
        val fullName = new Key('fullName) { type Value = String }  
        val isArrowType = new Key('isArrowType) { type Value = Boolean }
        override val tokenType = new Key('tokenType) { type Value = ScalaTokenType }
        val typeId = new Key('typeId) { type Value = Int }
        val outerTypeId = new Key('outerTypeId) { type Value = Int }
        
        val args = new Key('args){ type Value = String }
        val typeArgs = new Key('typeArgs){ type Value = String }
        val members = new Key('members){ type Value = String }
        
        val symbolDesignation = new Key('symbolDesignation){ type Value = org.codeprose.api.ScalaLang.SourceSymbol.SourceSymbol }
    
  //    val implicitConversion_indicator = new Key('implicitConversion_indicator){ type Value = Boolean }
  //    val implicitConversion_sourcePosition = new Key('implicitConversion_sourcePosition){ type Value = SourcePosition }
  //    val implicitConversion_sourcePositionWithTokenId = new Key('implicitConversion_sourcePosition){ type Value = SourcePositionWithTokenId }
  //    val implicitConversion_fullName = new Key('implicitConversion_fullName){ type Value = String }
  //    val implicitConversion_argNames = new Key('implicitConversion_argNamesName){ type Value = String }
  //    
  //    val implicitParameter_indicator = new Key('implicitParameter_indicator){ type Value = Boolean }
  //    val implicitParameter_sourcePosition = new Key('implicitParameter_sourcePosition){ type Value = SourcePosition }
  //    val implicitParameter_fullName = new Key('implicitParameter_fullname){ type Value = String }
  //    val implicitParameter_sourcePositionWithTokenId = new Key('implicitParameter_sourcePositionWithTokenId){ type Value = SourcePositionWithTokenId }
  
        val whereUsedWithInFile = new Key('whereUsedWithinFile){ type Value = List[Int] }
        
  //    val whereUsed = new Key('whereUsed){ type Value = List[ERangePosition]}
  //    val whereUsed_WithinFileTokenIdSrcPos = new Key('whereUsed_WithinFileTokenIdSrcPos){ type Value = List[SourcePositionWithTokenId]}
   
    //}
 

    
        
      // Keys Summary
      // ============================================================================
    
      val fileList = new Key('files){ type Value = List[java.io.File] }
      val typeInformation = new Key('typeInformation){ type Value = Map[Int,Option[TypeInformation]] }
      val whereUsedByTypeId = new Key('whereUsedByTypeId){ type Value = Map[Int,List[ERangePositionWithTokenIds]] }
    
  
}

object ScalaLang extends ScalaLang
