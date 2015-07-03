package org.codeprose.api

import org.scalatest.FunSpec
import org.codeprose.api.TokenPropertyMapOps

class TokenPropertyMapOpsSpec extends FunSpec {
  
  
  describe("A TokenPropertyMapOpsSpec "){
    
    it("should return the elements in the TPM ()"){
      import org.codeprose.api.{ScalaLangKeys => Keys}
      
      val tpm = new TokenPropertyMap()
      tpm.set(Keys.offset)(42)
      tpm.set(Keys.text)("text")
      
      // TODO: Does not compile.
      //import org.codeprose.api.{ScalaLangKeys => Keys}             
//      val tpmOps = new TokenPropertyMapOps(tpm)
//      tpmOps.getOffset() match {
//        case Some(o) => assert(o==42)
//        case None => fail()
//      }
//      
//      tpmOps.getText() match {
//        case Some(t) => assert(t=="text")
//        case None => fail()
//      }
//      
      
    }
    
    
  }

}