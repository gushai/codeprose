package org.codeprose.provider

import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token
import org.codeprose.api.scalalang._
import java.io.File
import com.typesafe.scalalogging.LazyLogging


/**
 * Tokenizes scala source code with scalariform and converts the tokens into
 * codeprose tokens.
 * 
 */
trait ScalaTokenizer extends Tokenizer with LazyLogging {
  
  /**
   * Tokenizes scala source code.
   * @param src Scala source code.
   * @return    Codeprose tokens.
   */
  def tokenize(src: String) : ArrayBuffer[Token] = {    
    val otherTokens = getTokensWithDifferentSpec(src)
    transformTokens(otherTokens)
  }

  /**
   * Generates underlying tokens, n this case scalariform tokens.
   * @param src Scala source code.
   * @return    Scalariform scala tokens.
   */
  private def getTokensWithDifferentSpec(src: String) : List[scalariform.lexer.Token] = {
      import scalariform.lexer.Token
      import scalariform.lexer.{ScalaLexer => Lexer}
      Lexer.rawTokenise(src)        
  }
  
  /**
   * Transforms scalariform tokens into codeprose tokens.
   * @param otherTokens Scalariform tokens.
   * @return            Codeprose tokens.
   */
  private def transformTokens(otherTokens: List[scalariform.lexer.Token]) : ArrayBuffer[org.codeprose.api.Token] = {
    val tokens = ArrayBuffer[org.codeprose.api.Token]()
    for( t<-otherTokens){         
            tokens += (convertIndividualToken(t))
          }    
      tokens
  }

  /**
   * Converts scalariform tokens into codeprose tokens.
   * @param t Scalariform token.
   * @return  Codeprose token.
   */
  private def convertIndividualToken(t: scalariform.lexer.Token) : Token = {
      val token = new org.codeprose.api.Token(t.offset,t.rawText)      
      token.set(ScalaLang.tokenType)(getTokenType(t))
      token
  }
  
  /**
   * Maps from scalariform token type to codeprose scala token type.
   * @param t Scalariform token.
   * @reutrn  Codeprose Scala token type.
   */
  private def getTokenType(t: scalariform.lexer.Token) : ScalaLang.ScalaTokenType = {
   import ScalaLang._ 
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

