package org.codeprose.api.TokenProperties

class SourcePosition(val filename: String, val offset: Int){
  override def toString() : String = {
    s"""($filename,$offset)"""
  }
}

class ArgumentList(){
  ???
}


  
