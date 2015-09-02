package org.codeprose.api


trait SourcePosition {}

case class OffsetSourcePosition(val filename: String, val offset: Int) extends SourcePosition {
   override def toString() : String = { s"""($filename,$offset)""" }
}

case class OffsetSourcePositionWithTokenId(
    val filename: String, val offset: Int, val tokenId: Int) extends SourcePosition {
  override def toString() : String = { s"""($filename,$offset,$tokenId)""" }
}

case class SourcePositionLinkWithCodeSample(
    srcFilename: String,
    link: String,
    tokenId: Int,
    sourceSample: List[String]){
  override def toString() : String = { val sample = sourceSample.mkString("") 
    s"""($srcFilename,$link,$tokenId,$sample)""" }
}
