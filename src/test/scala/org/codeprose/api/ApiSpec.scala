package org.codeprose.api

import org.scalatest.FunSpec
import com.google.common.base.Ticker

class ApiSpec extends FunSpec {

  describe("The Provider <-> Broker api"){
    
    val file = new java.io.File("/pathToFile.scala")
    
    val text = s"""
object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")
  }
}"""
    
    // Scalariform ScalaTokens
    val otherTokens = scalariform.lexer.ScalaLexer.rawTokenise(text)
    val tokens = org.codeprose.provider.util.ScalaTokenizer.tokenize(text)
       
    
    it("should print all tokens"){
      println("Available tokens:")
      tokens.foreach { e => println(e.toPrettyString()) }
    }
    
    it("should generate a ..."){
      
      import org.codeprose.api.scalalang.ScalaLang._
      
      
      assert(tokens.size == otherTokens.length)
      
      for (i <- 0 to tokens.size-1){
        assert(tokens(i).offset == otherTokens(i).offset)    
        assert(tokens(i).text == otherTokens(i).text)
        tokens(i)(tokenType) match {
          case Some(tt) => assert(tokens(i)(tokenType).get.toString == otherTokens(i).tokenType.toString)
          case None => fail()
        }        
        //println(tokens(i)(tokenType).toString + " : " + otherTokens(i).tokenType.toString)
      }
            
    }
    
    it("should fail..."){
     fail() 
    }
  }
  
}