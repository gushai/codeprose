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
    val tokens = org.codeprose.provider.Tokenizer.tokenize(text)
    
    it("should print all tokens"){
      tokens.foreach { e => println(e.toString()) }
    }
    
    it("should generate a ..."){
      import org.codeprose.api.ScalaLangKeys
      import org.codeprose.api.ScalaLang._
      
      val fileTokenInfo = scala.collection.mutable.Map[java.io.File,scala.collection.mutable.ArrayBuffer[(Int,TokenPropertyMap)]]()
      
      
      val tokenInfo = scala.collection.mutable.ArrayBuffer[(Int,TokenPropertyMap)]()      
      for(t<-tokens){
        
        val tpm = new TokenPropertyMap()
        tpm.set(ScalaLangKeys.offset)(t.offset)
        tpm.set(ScalaLangKeys.text)(t.rawText)
        
        //tpm.set(ScalaLangKeys.tokenType)()
//        t match {
//          case _ => ??? 
//        }
        
        tokenInfo         
      }
      
      fileTokenInfo += (file -> tokenInfo)
      
      fail()           
    }
    
  }
  
}