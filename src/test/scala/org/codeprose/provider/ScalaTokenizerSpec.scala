package org.codeprose.provider

import org.scalatest.FunSpec
import javax.print.attribute.standard.MediaSize.Other


class ScalaTokenizerSpec extends FunSpec {

  describe("A ScalaTokenizer"){
    
    it("should return language specific tokens"){
      
      import org.codeprose.provider.ScalaTokenizer
      
      val source = s"""
        /*
        * Multi 
        * line
        * comment
        */
        // single line comment
        object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")
  }
}"""
   
      // Tokenize (Scalariform) for comparison       
      val otherTokens = scalariform.lexer.ScalaLexer.rawTokenise(source)
            
      // Tokenize (internal format)      
      val tokens = ScalaTokenizer.tokenize(source)
      import org.codeprose.api.scalalang.ScalaLang._      
      
      assert(otherTokens.length == tokens.length)     

      // Compare tokens individually
      for(i <- 0 to tokens.length-1){

        assert(otherTokens(i).offset == tokens(i).offset)
        assert(otherTokens(i).rawText == tokens(i).text)
        
        //val tokenTypeName = tokens(i)(tokenType).map(tt => tt.name)
        tokens(i)(tokenType) match {
          case Some(tt) => {
          println(otherTokens(i).tokenType + " // " + tt.name)
          assert(tt.name == otherTokens(i).tokenType.name)
          }
          case None => fail()
        }
      }
    }
    
    it("should match MULTILINE_COMMENT tokentype ") { 
      
      val source = s"""
        /*
        * Multi 
        * line
        * comment
        */      
        /**
        * Multi 
        * line
        * comment scala doc
        */
"""       
      val otherTokens = scalariform.lexer.ScalaLexer.rawTokenise(source)
            
      // Tokenize (internal format)     
      import org.codeprose.provider.ScalaTokenizer
      val tokens = ScalaTokenizer.tokenize(source)      
      tokens.foreach(println)      
      import org.codeprose.api.scalalang.ScalaLang._     
      assert(tokens(0)(tokenType).get == Tokens.WS)
      assert(tokens(1)(tokenType).get == Tokens.MULTILINE_COMMENT)
      assert(tokens(2)(tokenType).get == Tokens.WS)
      assert(tokens(3)(tokenType).get == Tokens.MULTILINE_COMMENT)
      assert(tokens(4)(tokenType).get == Tokens.WS)
      assert(tokens(5)(tokenType).get == Tokens.EOF)      
    }
    
  }
  
  
}