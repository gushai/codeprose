package org.codeprose.provider


import scala.collection.mutable.ArrayBuffer
import java.io.File
import com.typesafe.scalalogging.LazyLogging
import org.codeprose.api.TokenPropertyMap

trait Tokenizer {
  def tokenize(src: String) : ArrayBuffer[org.codeprose.api.Token]
}

trait ScalaTokenizer extends Tokenizer with LazyLogging {
  
  import org.codeprose.api.ScalaLang
  
  def tokenize(src: String) : ArrayBuffer[org.codeprose.api.Token] = {    
    val otherTokens = getTokensWithDifferentSpec(src)
    transformTokens(otherTokens)
  }

  private def getTokensWithDifferentSpec(src: String) : List[scalariform.lexer.Token] = {
      logger.info("Creating scalariform tokens.")
      import scalariform.lexer.Token
      import scalariform.lexer.{ScalaLexer => Lexer}
      Lexer.rawTokenise(src)        
  }
  
  private def transformTokens(otherTokens: List[scalariform.lexer.Token]) : ArrayBuffer[org.codeprose.api.Token] = {
    logger.info("Transforming to codeprose tokens.")
    val tokens = ArrayBuffer[org.codeprose.api.Token]()
    for( t<-otherTokens){         
            tokens += (convertIndividualToken(t))
          }    
      tokens
  }

  private def convertIndividualToken(t: scalariform.lexer.Token) : org.codeprose.api.Token = {
      import org.codeprose.api.ScalaLang._
      val token = new org.codeprose.api.Token(t.offset,t.rawText)      
      token.set(tokenType)(getTokenType(t))
      token
  }
  
  private def getTokenType(t: scalariform.lexer.Token) : org.codeprose.api.ScalaLang.ScalaTokenType = {
   import org.codeprose.api.ScalaLang._ 
      t.tokenType match {
      case scalariform.lexer.Tokens.PACKAGE => {ScalaTokenType("PACKAGE")}
      case scalariform.lexer.Tokens.STAR => { ScalaTokenType("STAR") }
      case scalariform.lexer.Tokens.WHILE => { ScalaTokenType("WHILE") }
      case scalariform.lexer.Tokens.CASE => { ScalaTokenType("CASE") }
      case scalariform.lexer.Tokens.NEW => { ScalaTokenType("NEW") }
      case scalariform.lexer.Tokens.DO => { ScalaTokenType("DO") }
      case scalariform.lexer.Tokens.EQUALS => { ScalaTokenType("EQUALS") }
      case scalariform.lexer.Tokens.SUBTYPE => { ScalaTokenType("SUBTYPE") }
      case scalariform.lexer.Tokens.EOF => { ScalaTokenType("EOF") }
      case scalariform.lexer.Tokens.SEALED => { ScalaTokenType("SEALED") }
      case scalariform.lexer.Tokens.TYPE => { ScalaTokenType("TYPE") }
      case scalariform.lexer.Tokens.LBRACKET => { ScalaTokenType("LBRACKET") }
      case scalariform.lexer.Tokens.FINAL => { ScalaTokenType("FINAL") }
      case scalariform.lexer.Tokens.RPAREN => { ScalaTokenType("RPAREN") }
      case scalariform.lexer.Tokens.IMPORT => { ScalaTokenType("IMPORT") }
      case scalariform.lexer.Tokens.STRING_LITERAL => { ScalaTokenType("STRING_LITERAL") }
      case scalariform.lexer.Tokens.STRING_PART => { ScalaTokenType("STRING_PART") }
      case scalariform.lexer.Tokens.FLOATING_POINT_LITERAL => { ScalaTokenType("FLOATING_POINT_LITERAL") }
      case scalariform.lexer.Tokens.EXCLAMATION => { ScalaTokenType("EXCLAMATION") }
      case scalariform.lexer.Tokens.NEWLINES => { ScalaTokenType("NEWLINES") }
      case scalariform.lexer.Tokens.THIS => { ScalaTokenType("THIS") }
      case scalariform.lexer.Tokens.RETURN => { ScalaTokenType("RETURN") }
      case scalariform.lexer.Tokens.VAL => { ScalaTokenType("VAL") }
      case scalariform.lexer.Tokens.VAR => { ScalaTokenType("VAR") }
      case scalariform.lexer.Tokens.SUPER => { ScalaTokenType("SUPER") }
      case scalariform.lexer.Tokens.RBRACE => { ScalaTokenType("RBRACE") }
      case scalariform.lexer.Tokens.LINE_COMMENT => { ScalaTokenType("LINE_COMMENT") }
      case scalariform.lexer.Tokens.PRIVATE => { ScalaTokenType("PRIVATE") }
      case scalariform.lexer.Tokens.NULL => { ScalaTokenType("NULL") }
      case scalariform.lexer.Tokens.ELSE => { ScalaTokenType("ELSE") }
      case scalariform.lexer.Tokens.CHARACTER_LITERAL => { ScalaTokenType("CHARACTER_LITERAL") }
      case scalariform.lexer.Tokens.MATCH => { ScalaTokenType("MATCH") }
      case scalariform.lexer.Tokens.TRY => { ScalaTokenType("TRY") }
      case scalariform.lexer.Tokens.WS => { ScalaTokenType("WS") }
      case scalariform.lexer.Tokens.SUPERTYPE => { ScalaTokenType("SUPERTYPE") }
      case scalariform.lexer.Tokens.INTEGER_LITERAL => { ScalaTokenType("INTEGER_LITERAL") }
      case scalariform.lexer.Tokens.OP => { ScalaTokenType("OP") }
      case scalariform.lexer.Tokens.USCORE => { ScalaTokenType("USCORE") }
      case scalariform.lexer.Tokens.LOWER => { ScalaTokenType("LOWER") }
      case scalariform.lexer.Tokens.CATCH => { ScalaTokenType("CATCH") }
      case scalariform.lexer.Tokens.FALSE => { ScalaTokenType("FALSE") }
      case scalariform.lexer.Tokens.VARID => { ScalaTokenType("VARID") }
      case scalariform.lexer.Tokens.THROW => { ScalaTokenType("THROW") }
      case scalariform.lexer.Tokens.UPPER => { ScalaTokenType("UPPER") }
      case scalariform.lexer.Tokens.PROTECTED => { ScalaTokenType("PROTECTED") }
      case scalariform.lexer.Tokens.CLASS => { ScalaTokenType("CLASS") }
      case scalariform.lexer.Tokens.DEF => { ScalaTokenType("DEF") }
      case scalariform.lexer.Tokens.LBRACE => { ScalaTokenType("LBRACE") }
      case scalariform.lexer.Tokens.FOR => { ScalaTokenType("FOR") }
      case scalariform.lexer.Tokens.LARROW => { ScalaTokenType("LARROW") }
      case scalariform.lexer.Tokens.RARROW => { ScalaTokenType("RARROW") }
      case scalariform.lexer.Tokens.ABSTRACT => { ScalaTokenType("ABSTRACT") }
      case scalariform.lexer.Tokens.LPAREN => { ScalaTokenType("LPAREN") }
      case scalariform.lexer.Tokens.IF => { ScalaTokenType("IF") }
      case scalariform.lexer.Tokens.AT => { ScalaTokenType("AT") }
      case scalariform.lexer.Tokens.MULTILINE_COMMENT => { ScalaTokenType("MULTILINE_COMMENT") }
      case scalariform.lexer.Tokens.SYMBOL_LITERAL => { ScalaTokenType("SYMBOL_LITERAL") }
      case scalariform.lexer.Tokens.OBJECT => { ScalaTokenType("OBJECT") }
      case scalariform.lexer.Tokens.COMMA => { ScalaTokenType("COMMA") }
      case scalariform.lexer.Tokens.YIELD => { ScalaTokenType("YIELD") }
      case scalariform.lexer.Tokens.TILDE => { ScalaTokenType("TILDE") }
      case scalariform.lexer.Tokens.PLUS => { ScalaTokenType("PLUS") }
      case scalariform.lexer.Tokens.PIPE => { ScalaTokenType("PIPE") }
      case scalariform.lexer.Tokens.VIEWBOUND => { ScalaTokenType("VIEWBOUND") }
      case scalariform.lexer.Tokens.RBRACKET => { ScalaTokenType("RBRACKET") }
      case scalariform.lexer.Tokens.DOT => { ScalaTokenType("DOT") }
      case scalariform.lexer.Tokens.WITH => { ScalaTokenType("WITH") }
      case scalariform.lexer.Tokens.IMPLICIT => { ScalaTokenType("IMPLICIT") }
      case scalariform.lexer.Tokens.LAZY => { ScalaTokenType("LAZY") }
      case scalariform.lexer.Tokens.TRAIT => { ScalaTokenType("TRAIT") }
      case scalariform.lexer.Tokens.HASH => { ScalaTokenType("HASH") }
      case scalariform.lexer.Tokens.FORSOME => { ScalaTokenType("FORSOME") }
      case scalariform.lexer.Tokens.MINUS => { ScalaTokenType("MINUS") }
      case scalariform.lexer.Tokens.TRUE => { ScalaTokenType("TRUE") }
      case scalariform.lexer.Tokens.SEMI => { ScalaTokenType("SEMI") }
      case scalariform.lexer.Tokens.COLON => { ScalaTokenType("COLON") }
      case scalariform.lexer.Tokens.OTHERID => { ScalaTokenType("OTHERID") }
      case scalariform.lexer.Tokens.NEWLINE => { ScalaTokenType("NEWLINE") }
      case scalariform.lexer.Tokens.FINALLY => { ScalaTokenType("FINALLY") }
      case scalariform.lexer.Tokens.OVERRIDE => { ScalaTokenType("OVERRIDE") }
      case scalariform.lexer.Tokens.ARROW => { ScalaTokenType("ARROW") }
      case scalariform.lexer.Tokens.EXTENDS => { ScalaTokenType("EXTENDS") }
      case scalariform.lexer.Tokens.INTERPOLATION_ID => {ScalaTokenType("INTERPOLATION_ID", true) }
      case scalariform.lexer.Tokens.XML_START_OPEN => {ScalaTokenType("XML_START_OPEN", true) }
      case scalariform.lexer.Tokens.XML_EMPTY_CLOSE => {ScalaTokenType("XML_EMPTY_CLOSE", true) }
      case scalariform.lexer.Tokens.XML_TAG_CLOSE => {ScalaTokenType("XML_TAG_CLOSE", true) }
      case scalariform.lexer.Tokens.XML_END_OPEN => {ScalaTokenType("XML_END_OPEN", true) }
      case scalariform.lexer.Tokens.XML_WHITESPACE => {ScalaTokenType("XML_WHITESPACE", true) }
      case scalariform.lexer.Tokens.XML_ATTR_EQ => {ScalaTokenType("XML_ATTR_EQ", true) }
      case scalariform.lexer.Tokens.XML_ATTR_VALUE => {ScalaTokenType("XML_ATTR_VALUE", true) }
      case scalariform.lexer.Tokens.XML_NAME => {ScalaTokenType("XML_NAME", true) }
      case scalariform.lexer.Tokens.XML_PCDATA => {ScalaTokenType("XML_PCDATA", true) }
      case scalariform.lexer.Tokens.XML_COMMENT => {ScalaTokenType("XML_COMMENT", true) }
      case scalariform.lexer.Tokens.XML_CDATA => {ScalaTokenType("XML_CDATA", true) }
      case scalariform.lexer.Tokens.XML_UNPARSED => {ScalaTokenType("XML_UNPARSED", true) }
      case scalariform.lexer.Tokens.XML_PROCESSING_INSTRUCTION => {ScalaTokenType("XML_PROCESSING_INSTRUCTION", true) }
      case _ => {
          logger.error("Unknown input token type detected!")
          throw new Exception("Ooops: Unknown Scala Token Type!")
        }
      }      
  }
  
}

object ScalaTokenizer extends ScalaTokenizer























object TokenizerOldReference {


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
}

