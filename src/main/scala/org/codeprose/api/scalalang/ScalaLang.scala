package org.codeprose.api.scalalang

import org.codeprose.util.DynamicPropertyMap._
import java.io.File
import org.codeprose.api._


object ScalaLang extends ScalaLang


/*
 * Contains information needed for processing Scala source code.
 * 
 * Included:
 *  - Scala token type
 *  - Scala tokens
 *  - Scala source symbols
 *  - Scala Keys for Token and Summary
 */
trait ScalaLang extends DefaultLang {

  
  /*
   * Scala token type. Copied from scalariform see https://github.com/mdr/scalariform
   */
  case class ScalaTokenType(val name: String, isXml: Boolean = false) extends TokenType {
  
    import ScalaLang.Tokens._
    def isNewline = this == Tokens.NEWLINE || this == Tokens.NEWLINES
    
    def isKeyword = Tokens.KEYWORDS contains this 
    def isComment = Tokens.COMMENTS contains this
    def isId = Tokens.IDS contains this
    def isLiteral = Tokens.LITERALS contains this
    
    override lazy val toString = name
  
  }
  
  /* 
   * Collection of Scala tokens.
   * 
   * Copied from scalariform see https://github.com/mdr/scalariform
   * (scalariform/scalariform/src/main/scala/com/danieltrinh/scalariform/lexer/Tokens.scala)
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
  
  
/*
 * Scala source symbols.
 * 
 * Copied from scalariform see https://github.com/mdr/scalariform
 */
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
  
  /**
   * Maps from ensime source to codeprose source symbol.
   * 
   * @param sourceSym org.ensime.api.SourceSymbol
   * @return SourceSymbol
   */
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
  
  // Key for Tokens
  // ==========================================================================

  /* 
   * Scala token type.       
   */
  override val tokenType = new Key('tokenType) { type Value = ScalaTokenType }
  /*
   * Scala types are references using a type id. 
   * Type ids can be used to reference information in the project's summary 
   * saved with key typeInspectInformation.
   */
  val typeId = new Key('typeId) { type Value = Int }
  
  /*
   * Each token is assigned a token id to allow easy reference in the output.
   */
  val internalTokenId = new Key('internalTokenId){ type Value = Int }
  
  /*
   * Used to save the point of declaration of a token.
   */
  val declaredAt = new Key('declaredAt) { type Value = OffsetSourcePositionWithTokenId }
  
  /*
   * Full type name.
   */
  val fullName = new Key('fullName) { type Value = String }
  
  /*
   * Used for semantic highlighting.
   */
  val symbolDesignation = new Key('symbolDesignation){ type Value = ScalaLang.SourceSymbol.SourceSymbol }
  
  /*
   * Indicator for implicit conversion at token.
   */
  val implicitConversionIndicator = new Key('implicitConversionIndicator){ type Value = Boolean }
  
  /*
   * Ids of implicit conversions at token.
   * Ids reference implicit conversion information saved in the projects summary 
   * with key ImplicitConversionInfoSummary.
   */
  val implicitConversionIds = new Key('implicitConversionIds){ type Value = List[Int] }
  
  /*
   * Indicator for implicit parameter at token.
   */
  val implicitParameterIndicator = new Key('implicitParameterIndicator){ type Value = Boolean }
  
  /*
   * Ids of implicit parameters at token.
   * Ids reference implicit parameter information saved in the projects summary
   * with key ImplicitParameterInfoSummary.
   */
  val implicitParameterIds = new Key('implicitParameterIds){ type Value = List[Int] }
  
  /*
   * List of source positions of the symbols occurrences in the same file. 
   */
  val whereUsedWithInFile = new Key('whereUsedWithinFile){ type Value = List[ERangePositionWithTokenId] }
  
  /*
   * Indicator for is callable of a token.
   */
  val isCallable = new Key('isCallable){ type Value = Boolean }
  
  /*
   * Owner type id of the token.
   * Same ids as typeId. 
   */
  val ownerTypeId = new Key('ownerTypeId){ type Value = Int }
  
  
  // Keys Summary
  // ============================================================================
  
  /*
   * List of files in the project
   */
  val fileList = new Key('files){ type Value = List[File] }
  
  /*
   * Map of type ids to source positions with occurrences of the type.
   */
  val whereUsedByTypeId = new Key('whereUsedByTypeId){ type Value = Map[Int,List[ERangePositionWithTokenId]] }
  
  /*
   * Map of type ids to source positions with occurrences of the type and code samples.
   */
  val whereUsedByTypeIdWithCodeSample = new Key('whereUsedByTypeIdWithCodeSample){ type Value = Map[Int,List[(ERangePositionWithTokenId, List[String])]] }
  
  /*
   * List of package names per file.
   */
  val packageNamePerFile = new Key('packageOfFiles){ type Value = Map[File,String] }
  
  /*
   * Map package information per package name.
   */
  val packageInformation = new Key('packageInformation){ type Value = Map[String,Option[PackageInfo]] }
  
  /*
   * Map type id to type inspect information.
   */
  val typeInspectInformation = new Key('typeInspectInformation){ type Value = Map[Int,Option[TypeInspectInfo]] }
  
  /*
   * Map from implicitConversionId to ImplicitConversionInfoSummary.
   */
  val implicitConversionInformation = new Key('implicitConversionInformation){ type Value = Map[Int,ImplicitConversionInfoSummary] }
  
  /*
   * Map from implicitParameterId to ImplicitParameterInfoSummary.
   */
  val implicitParameterInformation = new Key('implicitParameterInformation){ type Value = Map[Int,ImplicitParamInfoSummary] }
}


