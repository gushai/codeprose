package org.codeprose.api.TokenProperties

class SourcePosition(val filename: String, val offset: Int){
  override def toString() : String = {
    s"""($filename,$offset)"""
  }
}

class SourcePositionWithTokenId(val filename: String, val tokenId: Int){
override def toString() : String = {
    s"""($filename,$tokenId)"""
  }
}

class ERangePosition(val filename: String, val offset: Int, val start: Int, val end: Int){
  override def toString() : String = {
    s"""($filename,$offset,$start,$end)"""
  }
}


class ArgumentList(){
  ???
}


class ImplicitConversion(
    val fullname: String,
    val typeId: Int,
    val args: String,
    val typeArgs: String,
    val whereUsedWithinFile_OffsetBased: SourcePosition,
    var whereUsedWithinFile_TokenBase: SourcePositionWithTokenId
    ) {
  
}



  
