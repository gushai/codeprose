package org.codeprose.provider



// ==================================================================
// Initial PoC
// ==================================================================
// Remove - begin
import java.io.File
import com.typesafe.scalalogging.LazyLogging

// Remove - end

// ==================================================================
// New version
// ==================================================================
import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.TokenPropertyMap
import javax.print.attribute.standard.MediaSize.Other

trait Tokenizer {
  import scalariform.lexer.Token
 def tokenize(file: File) : List[Token]  
 def tokenize(src: String) : List[Token]
}


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
    val otherTokens = getTokens(src)
    logger.info("Conversion to CPTokens")
    
    val tokens = ArrayBuffer[(Int, TokenPropertyMap)]()
    
    import org.codeprose.api.ScalaLangKeys
    
    for( t<-otherTokens){
      val tpm = new TokenPropertyMap()
      tpm.set(ScalaLangKeys.offset)(t.offset)
      tpm.set(ScalaLangKeys.text)(t.rawText)
      tpm.set(ScalaLangKeys.tokenType)(getTokenType(t))      
      tokens += ((t.offset,tpm))
    }
    
    ???
  }
  
  private def getTokenType(token: Token) : org.codeprose.api.TokenType = {
    org.codeprose.api.ScalaTokenType(token.tokenType.name)
  }
}


