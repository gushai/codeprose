package org.codeprose.provider


import scala.collection.mutable.ArrayBuffer
import java.io.File
import com.typesafe.scalalogging.LazyLogging
import org.codeprose.api.TokenPropertyMap

// ==================================================================
// Initial PoC
// ==================================================================

// Remove - begin
object Tokenizer extends Tokenizer with LazyLogging {
	import scalariform.lexer.Token
	def tokenize(file: File): List[Token] = {
			logger.info("Tokenizing: \t" + file)
			val content = scala.io.Source.fromFile(file.getAbsolutePath(), "utf-8").getLines.mkString("\n")
			tokenize(content)
	}

	def tokenize(src: String): List[Token] = {
			logger.info("Tokenizing: \t" + src.take(25))
			import scalariform.lexer.{ScalaLexer => lexer}
			lexer.rawTokenise(src)     
	}
}

// Remove - end

// ==================================================================
// New version
// ==================================================================



trait Tokenizer {
	import scalariform.lexer.Token
	def tokenize(file: File) : List[Token]  
			def tokenize(src: String) : List[Token]
}


trait CPTokenizer {  
	def tokenize(src: String): ArrayBuffer[(Int,TokenPropertyMap)] 
}

object CPTokenizer extends CPTokenizer with LazyLogging {
	import scalariform.lexer.Token
	private def getTokens(src: String): List[Token] = {
			logger.info("Tokenizing: \t" + src.take(25))
			import scalariform.lexer.{ScalaLexer => lexer}
			lexer.rawTokenise(src)     
	}

	def tokenize(src: String): ArrayBuffer[(Int, TokenPropertyMap)] = {    
			// Get language specific tokens
			val otherTokens = getTokens(src)
					logger.info("Conversion to CPTokens")

					val tokens = ArrayBuffer[(Int, TokenPropertyMap)]()    
					for( t<-otherTokens){         
						tokens += ((t.offset,convertToCPToken(t)))
					}    
			tokens
	}

	private def convertToCPToken(token: Token) : TokenPropertyMap = {
			import org.codeprose.api.ScalaLangKeys
			val tpm = new TokenPropertyMap()
			tpm.set(ScalaLangKeys.offset)(token.offset)
			tpm.set(ScalaLangKeys.text)(token.rawText)
			tpm.set(ScalaLangKeys.tokenType)(getTokenType(token))
			tpm.set(ScalaLangKeys.token)(getToken(token))
			tpm
	}

	/**
	 * Returns language specific tokens.
	 */
	private def getTokenType(token: Token) : org.codeprose.api.TokenType = {
			org.codeprose.api.ScalaTokenType(token.tokenType.name)
	}

	/**
	 * Returns a ScalaToken
	 */
	private def getToken(token: Token) : org.codeprose.api.ScalaLang.ScalaToken = {

			import org.codeprose.api.ScalaLang 
			token.tokenType match {
			case scalariform.lexer.Tokens.PACKAGE => {ScalaLang.PACKAGE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.STAR => {ScalaLang.STAR(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.WHILE => {ScalaLang.WHILE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.CASE => {ScalaLang.CASE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.NEW => {ScalaLang.NEW(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.DO => {ScalaLang.DO(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.EQUALS => {ScalaLang.EQUALS(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.SUBTYPE => {ScalaLang.SUBTYPE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.EOF => {ScalaLang.EOF(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.SEALED => {ScalaLang.SEALED(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.TYPE => {ScalaLang.TYPE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.LBRACKET => {ScalaLang.LBRACKET(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.FINAL => {ScalaLang.FINAL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.RPAREN => {ScalaLang.RPAREN(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.IMPORT => {ScalaLang.IMPORT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.STRING_LITERAL => {ScalaLang.STRING_LITERAL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.STRING_PART => {ScalaLang.STRING_PART(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.FLOATING_POINT_LITERAL => {ScalaLang.FLOATING_POINT_LITERAL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.EXCLAMATION => {ScalaLang.EXCLAMATION(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.NEWLINES => {ScalaLang.NEWLINES(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.THIS => {ScalaLang.THIS(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.RETURN => {ScalaLang.RETURN(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.VAL => {ScalaLang.VAL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.VAR => {ScalaLang.VAR(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.SUPER => {ScalaLang.SUPER(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.RBRACE => {ScalaLang.RBRACE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.LINE_COMMENT => {ScalaLang.LINE_COMMENT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.PRIVATE => {ScalaLang.PRIVATE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.NULL => {ScalaLang.NULL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.ELSE => {ScalaLang.ELSE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.CHARACTER_LITERAL => {ScalaLang.CHARACTER_LITERAL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.MATCH => {ScalaLang.MATCH(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.TRY => {ScalaLang.TRY(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.WS => {ScalaLang.WS(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.SUPERTYPE => {ScalaLang.SUPERTYPE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.INTEGER_LITERAL => {ScalaLang.INTEGER_LITERAL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.OP => {ScalaLang.OP(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.USCORE => {ScalaLang.USCORE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.LOWER => {ScalaLang.LOWER(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.CATCH => {ScalaLang.CATCH(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.FALSE => {ScalaLang.FALSE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.VARID => {ScalaLang.VARID(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.THROW => {ScalaLang.THROW(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.UPPER => {ScalaLang.UPPER(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.PROTECTED => {ScalaLang.PROTECTED(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.CLASS => {ScalaLang.CLASS(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.DEF => {ScalaLang.DEF(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.LBRACE => {ScalaLang.LBRACE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.FOR => {ScalaLang.FOR(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.LARROW => {ScalaLang.LARROW(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.RARROW => {ScalaLang.RARROW(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.ABSTRACT => {ScalaLang.ABSTRACT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.LPAREN => {ScalaLang.LPAREN(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.IF => {ScalaLang.IF(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.AT => {ScalaLang.AT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.MULTILINE_COMMENT => {ScalaLang.MULTILINE_COMMENT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.SYMBOL_LITERAL => {ScalaLang.SYMBOL_LITERAL(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.OBJECT => {ScalaLang.OBJECT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.COMMA => {ScalaLang.COMMA(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.YIELD => {ScalaLang.YIELD(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.TILDE => {ScalaLang.TILDE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.PLUS => {ScalaLang.PLUS(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.PIPE => {ScalaLang.PIPE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.VIEWBOUND => {ScalaLang.VIEWBOUND(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.RBRACKET => {ScalaLang.RBRACKET(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.DOT => {ScalaLang.DOT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.WITH => {ScalaLang.WITH(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.IMPLICIT => {ScalaLang.IMPLICIT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.LAZY => {ScalaLang.LAZY(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.TRAIT => {ScalaLang.TRAIT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.HASH => {ScalaLang.HASH(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.FORSOME => {ScalaLang.FORSOME(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.MINUS => {ScalaLang.MINUS(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.TRUE => {ScalaLang.TRUE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.SEMI => {ScalaLang.SEMI(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.COLON => {ScalaLang.COLON(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.OTHERID => {ScalaLang.OTHERID(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.NEWLINE => {ScalaLang.NEWLINE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.FINALLY => {ScalaLang.FINALLY(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.OVERRIDE => {ScalaLang.OVERRIDE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.ARROW => {ScalaLang.ARROW(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.EXTENDS => {ScalaLang.EXTENDS(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.INTERPOLATION_ID => {ScalaLang.INTERPOLATION_ID(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_START_OPEN => {ScalaLang.XML_START_OPEN(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_EMPTY_CLOSE => {ScalaLang.XML_EMPTY_CLOSE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_TAG_CLOSE => {ScalaLang.XML_TAG_CLOSE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_END_OPEN => {ScalaLang.XML_END_OPEN(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_WHITESPACE => {ScalaLang.XML_WHITESPACE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_ATTR_EQ => {ScalaLang.XML_ATTR_EQ(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_ATTR_VALUE => {ScalaLang.XML_ATTR_VALUE(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_NAME => {ScalaLang.XML_NAME(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_PCDATA => {ScalaLang.XML_PCDATA(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_COMMENT => {ScalaLang.XML_COMMENT(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_CDATA => {ScalaLang.XML_CDATA(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_UNPARSED => {ScalaLang.XML_UNPARSED(token.offset, token.rawText)}
			case scalariform.lexer.Tokens.XML_PROCESSING_INSTRUCTION => {ScalaLang.XML_PROCESSING_INSTRUCTION(token.offset, token.rawText)}
			case _ => throw new Exception("Ooops: Unknown Scala Token Type!")
			}
	}

}


