package org.codeprose.consumer.util

import org.scalatest.FunSpec

/**
 * Test CommentUtil.
 */
class CommentUtilSpec extends FunSpec{
  
  describe("CommentUtil.cleanMultilineComment"){
    
    it("should remove the multiline comment signature with leading/trailing spaces"){
      import org.codeprose.consumer.util.CommentUtil
      
      val mlc =List(
s""" /*
* asdf
*/""",
s""" /*
*   asdf
*/ """,
s"""
 /*
  * asdf
*/
""",
s"""
 /*
  *   asdf
*/ """)
      
      for (c<-mlc){
        val cleaned = CommentUtil.cleanMultilineComment(c)
        assert(cleaned.trim == "asdf")
      }

    }
    
    it("should remove the multiline comment signature with different indentations"){
      fail()
    }
   
    it("should have much more rigorous testing..."){
        // TODO: Implement more tests
       fail()
    }
  }
  
  describe("CommentUtil.isScalaDocComment"){
    
    it("should have more testing..."){
      // TODO: Implement more tests
      fail()
    }
    
  }
  
}