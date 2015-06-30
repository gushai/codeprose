package org.codeprose.consumer.util

import org.scalatest.FunSpec


class MarkdownConverterSpec extends FunSpec{

  
  
   
  
  describe("A MarkdownConverter") {
    it("should handle headlines"){
      
      val input =List( 
s"""
Header1
=======
""",
s"""
Header2
-------
""",
s"""
### Header3
""")
          
     val result = List(
         s"""<h1 id = "Header1">Header1</h1>""",
         s"""<h2 id = "Header2">Header2</h2>""",
         s"""<h3 id = "Header3">Header3</h3>""")
              
     import org.codeprose.consumer.util.{MarkdownConverter=>mdConverter}
     for(i<- 0 to input.length-1){
       val output = mdConverter.apply(input(i))
       assert(output==result(i))
     }
    }
    
    ignore("should handle numerations"){
      
    }
    
    ignore("should handle ..."){
      
    }
  }
  
}