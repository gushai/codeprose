package org.codeprose.api

class TokenPropertyMapOps( tpm: TokenPropertyMap) (implicit keys: Keys) {
  def getText() : Option[String] = {
    tpm.get(keys.text) 
  }
  def getOffset() : Option[Int] = {
    tpm.get(keys.offset)
  }  
}



// TODO: Does not take the object as argument.
// Change the ScalaLangKeys specification to be a trait?

// class TokenPropertyMapScalaOps (implicit keys: ScalaLangKeys) {
//  def getFullName() : Option[String] = {
//  //  tpm.get(keys.fullName) 
//    ???
//  }
//  def getOffset() : Option[Int] = {
//    // tpm.get(keys.offset)
//    ???
//  }
// }