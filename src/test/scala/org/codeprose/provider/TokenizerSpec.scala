package org.codeprose.provider

import org.scalatest.FunSpec
import javax.print.attribute.standard.MediaSize.Other


class TokenizerSpec extends FunSpec {

  describe("A CPTokenizer"){
    
    it("should return codeprose TPM"){
      
      import org.codeprose.provider.CPTokenizer
      
       
      val source = s"""object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")
  }
}"""
   
      // Tokenize (Scalariform)
       
      val otherTokens = org.codeprose.provider.Tokenizer.tokenize(source)
            
      // Tokenize (internal format)      
      val tokens = CPTokenizer.tokenize(source)
      import org.codeprose.api.ScalaLangKeys      
      //tokens.map(e => e._2.get(ScalaLangKeys.tokenType)).foreach(println)
      
      assert(otherTokens.length == tokens.length)     
      for(i <- 0 to tokens.length-1){
        // Compare offset
        assert(otherTokens(i).offset == tokens(i)._1)
        
        // Compare text
        assert(otherTokens(i).rawText == tokens(i)._2.get(ScalaLangKeys.text).get)
        
        println(otherTokens(i).tokenType + " // " + tokens(i)._2.get(ScalaLangKeys.tokenType).get)
        
      }
      
      
      
      
      
    }
    
    
  }
  
  
}