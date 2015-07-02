package org.codeprose.api

import scala.collection.mutable.ArrayBuffer

// type alias for Collection used
object Api{
  type FileTokenCollection = scala.collection.mutable.Map[java.io.File,ArrayBuffer[(Int,TokenPropertyMap)]]
}



trait Language {

	trait DefaultToken {
		val offset: Int
		val text: String // In case of scala unicode escaping is not applied
		val length = text.length
    val range = Range(offset, offset+length)
	}

	type Token <: DefaultToken 

}

object ScalaLang extends Language {

	trait ScalaToken extends DefaultToken {
		type tokenType <: ScalaTokenType
		val tokenType : ScalaTokenType    
	}

	type TokenType <: ScalaToken 

case class PACKAGE(val offset: Int, val text: String) extends ScalaToken {
		val tokenType = ScalaTokenType("PACKAGE")
	}
case class STAR(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("STAR")
}
case class WHILE(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("WHILE") }

case class CASE(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("CASE")
}
case class NEW(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("NEW")
}
case class DO(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("DO")
}
case class EQUALS(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("EQUALS")
}
case class SUBTYPE(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SUBTYPE")
}
case class EOF(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("EOF")
}
case class SEALED(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SEALED")
}
case class TYPE(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("TYPE")
}
case class LBRACKET(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LBRACKET")
}
case class FINAL(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("FINAL") 
}
case class  RPAREN (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("RPAREN")
}
case class  IMPORT(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("IMPORT")
}
case class  STRING_LITERAL(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("STRING_LITERAL")
}
case class  STRING_PART(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("STRING_PART")
}
case class  FLOATING_POINT_LITERAL(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("FLOATING_POINT_LITERAL")
}
case class  EXCLAMATION(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("EXCLAMATION")
}
case class  NEWLINES(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("NEWLINES")
}
case class  THIS(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("THIS")
}
case class  RETURN (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("RETURN")
}
case class  VAL (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("VAL")
}
case class  VAR (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("VAR")
}
case class  SUPER (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SUPER")
}
case class  RBRACE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("RBRACE")
}
case class  LINE_COMMENT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LINE_COMMENT")
}
case class  PRIVATE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("PRIVATE")
}
case class  NULL (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("NULL")
}
case class  ELSE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("ELSE")
}
case class  CHARACTER_LITERAL (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("CHARACTER_LITERAL")
}
case class  MATCH (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("MATCH")
}
case class  TRY (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("TRY")
}
case class  WS (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("WS")
}
case class  SUPERTYPE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SUPERTYPE")
}
case class  INTEGER_LITERAL (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("INTEGER_LITERAL")
}
case class  OP (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("OP")
}
case class  USCORE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("USCORE")
}
case class  LOWER (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LOWER")
}
case class  CATCH (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("CATCH")
}
case class  FALSE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("FALSE")
}
case class  VARID (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("VARID")
}
case class  THROW (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("THROW")
}
case class  UPPER (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("UPPER")
}
case class  PROTECTED (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("PROTECTED")
}
case class  CLASS (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("CLASS")
}
case class  DEF (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("DEF")
}
case class  LBRACE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LBRACE")
}
case class  FOR (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("FOR")
}
case class  LARROW (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LARROW")
}
case class  RARROW (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("RARROW")
}
case class  ABSTRACT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("ABSTRACT")
}
case class  LPAREN (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LPAREN")
}
case class  IF (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("IF")
}
case class  AT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("AT")
}
case class  MULTILINE_COMMENT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("MULTILINE_COMMENT")
}
case class  SYMBOL_LITERAL (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SYMBOL_LITERAL")
}
case class  OBJECT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("OBJECT")
}
case class  COMMA (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("COMMA")
}
case class  YIELD (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("YIELD")
}
case class  TILDE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("TILDE")
}
case class  PLUS (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("PLUS")
}
case class  PIPE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("PIPE")
}
case class  VIEWBOUND (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("VIEWBOUND")
}
case class  RBRACKET (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("RBRACKET")
}
case class  DOT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("DOT")
}
case class  WITH (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("WITH")
}
case class  IMPLICIT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("IMPLICIT")
}
case class  LAZY (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LAZY")
}
case class  TRAIT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("TRAIT")
}
case class  HASH (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("HASH")
}
case class  FORSOME (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("FORSOME")
}
case class  MINUS (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("MINUS")
}
case class  TRUE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("TRUE")
}
case class  SEMI (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SEMI")
}
case class  COLON (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("COLON")
}
case class  OTHERID (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("OTHERID")
}
case class  NEWLINE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("NEWLINE")
}
case class  FINALLY (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("FINALLY")
}
case class  OVERRIDE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("OVERRIDE")
}
case class  ARROW (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("ARROW")
}
case class  EXTENDS (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("EXTENDS")
}
case class  INTERPOLATION_ID (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("INTERPOLATION_ID")
}
case class  XML_START_OPEN (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_START_OPEN", isXml = true)
}
case class  XML_EMPTY_CLOSE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_EMPTY_CLOSE", isXml = true)
}
case class  XML_TAG_CLOSE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_TAG_CLOSE", isXml = true)
}
case class  XML_END_OPEN (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_END_OPEN", isXml = true)
}
case class  XML_WHITESPACE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_WHITESPACE", isXml = true)
}
case class  XML_ATTR_EQ (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_ATTR_EQ", isXml = true)
}
case class  XML_ATTR_VALUE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_ATTR_VALUE", isXml = true)
}
case class  XML_NAME (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_NAME", isXml = true)
}
case class  XML_PCDATA (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_PCDATA", isXml = true)
}
case class  XML_COMMENT (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_COMMENT", isXml = true)
}
case class  XML_CDATA (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_CDATA", isXml = true)
}
case class  XML_UNPARSED (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_UNPARSED", isXml = true)
}
case class  XML_PROCESSING_INSTRUCTION (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("XML_PROCESSING_INSTRUCTION", isXml = true)
}


}










