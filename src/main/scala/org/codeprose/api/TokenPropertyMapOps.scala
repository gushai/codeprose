//package org.codeprose.api
//
//class TokenPropertyMapOps( tpm: TokenPropertyMap) (implicit keys: Keys) {
//  def getText() : Option[String] = {
//    tpm.get(keys.text) 
//  }
//  def getOffset() : Option[Int] = {
//    tpm.get(keys.offset)
//  }  
//}
//
//
//
//// TODO: Does not take the object as argument.
//// Change the ScalaLangKeys specification to be a trait?
//
//// Add scala singleton type!
// class TokenPropertyMapScalaOps(tpm: TokenPropertyMap) (implicit keys: ScalaLangKeys.type) {
//  def getFullName() : Option[String] = {
//    tpm.get(keys.fullName) 
//  }
//  def getOffset() : Option[Int] = {
//    // tpm.get(keys.offset)
//    ???
//  }
// }