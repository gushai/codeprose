package org.codeprose.api

import org.codeprose.api.TokenProperties.SourcePosition
import org.codeprose.api.TokenProperties.SourcePositionWithTokenId

class TypeInformation(
    val typeId: Int, 
    val fullname: String, 
    val declaredAs: String,
    val declaredAt: SourcePosition,
    val declaredAt_TokenIdSrcPos: SourcePositionWithTokenId,
    val args: List[String],
    val typeArgs: List[String],
    val members: List[InterfaceInfo]) {
  
}


class NamedTypeMemberInfo(){
  
}

class InterfaceInfo(
  val typeId: Int,
  val fullname: String,
  val signature: String,
  val declaredAt: SourcePosition,
  val declaredAt_TokenIdSrcPos: SourcePositionWithTokenId,
  val members: List[NamedTypeMemberInfo]
  ){
  
}

