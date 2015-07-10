package org.codeprose.consumer.util

import org.scalatest.FunSpec

class CommentUtilSpec extends FunSpec{
  
  describe("CommentUtil"){
    
    it("should remove the multiline comment signature with leading/trailing spaces"){
      import org.codeprose.consumer.util.CommentUtil
      
      val mlc =List(
s"""/*
* asdf
*/""",
s""" /*
* asdf
*/ """,
s"""
 /*
* asdf
*/
""")
      
  fail()
//    for (c<-mlc){
//      println(CommentUtil.cleanMultilineComment(c))
//      assert(c=="asdf")
//      
//    }

    }
    
    it("should remove the multiline comment signature with different indentations"){
      fail()
    }
    
  }
  
}