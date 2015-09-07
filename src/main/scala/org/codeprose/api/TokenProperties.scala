package org.codeprose.api

/*
 * Contains language independent classes to enrich tokens.
 */
trait SourcePosition {}

/**
 * Offset source position.
 * @param filename  Full filename.
 * @param offset    Offset in the source file.
 */
case class OffsetSourcePosition(val filename: String, val offset: Int) extends SourcePosition {
   override def toString() : String = { s"""($filename,$offset)""" }
} 

/**
 * Offset source position with token id.
 * @param   filename  Full filename.
 * @param   offset    Offset in the source file.
 * @tokenId           Token id of the token found at offset in filename.
 */
case class OffsetSourcePositionWithTokenId(
    val filename: String, val offset: Int, val tokenId: Int) extends SourcePosition {
  override def toString() : String = { s"""($filename,$offset,$tokenId)""" }
}

/**
 * Source position with source sample.
 * @param srcFilename   Filename
 * @param link          ??
 * @param tokenId       Token id identifying the source position.
 * @param sourceSample  List of strings making up the source sample. 
 *                      List mostly filled with: lines before, source position, lines after.
 */
case class SourcePositionLinkWithCodeSample(
    srcFilename: String,
    link: String,
    tokenId: Int,
    sourceSample: List[String]){
  override def toString() : String = { val sample = sourceSample.mkString("") 
    s"""($srcFilename,$link,$tokenId,$sample)""" }
}
