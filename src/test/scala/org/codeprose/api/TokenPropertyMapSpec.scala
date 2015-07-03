package org.codeprose.api

import org.scalatest.FunSpec

class TokenPropertyMapSpec extends FunSpec {

  describe("A TokenPropertyMap"){
    
    it("should be able to added and remove key value pairs"){
      
      val tpm = new TokenPropertyMap()      
      import org.codeprose.api.ScalaLangKeys
      import org.codeprose.api.TokenProperties._
      
      // Add stuff
      tpm.set(ScalaLangKeys.offset)(42)
      assert(tpm.size() == 1)
      tpm.set(ScalaLangKeys.text)("text")
      assert(tpm.size() == 2)
      tpm.set(ScalaLangKeys.typeId)(24)
      assert(tpm.size() == 3)
      tpm.set(ScalaLangKeys.declaredAt)(new SourcePosition("pathtofile",42))
      assert(tpm.size() == 4)      
      tpm.set(ScalaLangKeys.fullName)("full.name")
      assert(tpm.size() == 5)
      
                  
      // Get stuff and remove
      tpm.get(ScalaLangKeys.typeId) match {
        case Some(id) => assert(id==24)
        case None => fail()
      }      
      tpm.remove(ScalaLangKeys.typeId)
      
      tpm.get(ScalaLangKeys.offset) match{
        case Some(offset)  => assert(offset == 42)
        case None => fail()
      }
      tpm.remove(ScalaLangKeys.offset)
      
      tpm.get(ScalaLangKeys.text) match{
        case Some(s)  => assert(s == "text")
        case None => fail()
      }
      tpm.remove(ScalaLangKeys.text)
      
      val sp = tpm.get(ScalaLangKeys.declaredAt)
      sp match {
        case Some(sp) => assert(sp.filename == "pathtofile" && sp.offset == 42)
        case None => fail()
      }
      tpm.remove(ScalaLangKeys.declaredAt)
            
      tpm.get(ScalaLangKeys.fullName) match {
        case Some(name) => assert(name == "full.name")
        case None => fail()
      }
      tpm.remove(ScalaLangKeys.fullName)
      
      assert(tpm.size() == 0)
    }
    
  }
  
  
  
  
}