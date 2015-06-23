package org.codeprose.util

import org.scalatest.FunSpec


class StringUtilSpec extends FunSpec {

  
  describe("StringUtil.findLongtestCommonPrefix()"){
  
    import org.codeprose.util.StringUtil
    
    it("should return an empty string for empty input:"){
    
        assert(StringUtil.findLongtestCommonPrefix(List[String]()).equals(""))
     }
    
    it("should return an empty string for inputs with no common prefix (2 inputs):"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","1234")).equals(""))
     }
    
    
    it("should return an empty string for inputs with no common prefix (3 inputs):"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","1234","ASFD")).equals(""))
     }
    
    it("should return correct prefix"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","as1211","asDF1")).equals("as"))
    }
    
    it("should return prefix with input all the same"){    
        assert(StringUtil.findLongtestCommonPrefix(List("asdf","asdf","asdf")).equals("asdf"))
    }
    
  }
  
  describe(""){
    it("should fail"){
     fail()
    }
  }
}