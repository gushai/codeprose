package org.codeprose.provider

import org.scalatest.FunSpec
import javax.print.attribute.standard.MediaSize.Other


class ScalaTokenizerSpec extends FunSpec {

  describe("A ScalaTokenizer"){
    
    it("should return language specific tokens"){
      
      import org.codeprose.provider.ScalaTokenizer
      
      val source = s"""object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")
  }
}"""
   
      // Tokenize (Scalariform) for comparison       
      val otherTokens = scalariform.lexer.ScalaLexer.rawTokenise(source)
            
      // Tokenize (internal format)      
      val tokens = ScalaTokenizer.tokenize(source)
      import org.codeprose.api.ScalaLang._      
      
      assert(otherTokens.length == tokens.length)     

      // Compare tokens individually
      for(i <- 0 to tokens.length-1){

        assert(otherTokens(i).offset == tokens(i).offset)
        assert(otherTokens(i).rawText == tokens(i).text)
        
        val tokenTypeName = tokens(i)(tokenType).map(tt => tt.name)
        println(otherTokens(i).tokenType + " // " + tokenTypeName)
        
      }
      
      
      
      
      
    }
    
    
  }
  
  
}