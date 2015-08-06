package org.codeprose.consumer.util

import org.codeprose.api.Token

/**
 * @author gus
 */



object ConsumerHelper {
  
  def generateMappingFileOffsetToLine(tokens: Array[Token]) : Array[(Int,Int)] = {
    
    var totalOffset = 0    
    tokens.map(t=>t.text).mkString.split("\n").map({
      line => {
        val totalOffsetLag = totalOffset
        totalOffset = totalOffset + line.length
        (totalOffsetLag,totalOffset)
        }
      }) 
  }
  /**
   * Translates an absolute offset to a line number and relative offset.
   * @param 
   * @return (Int,Int) Line index and relative offset in line.
   */
  def mapAbsoluteOffsetToLineWithOffset(absOffset: Int)(implicit lineOffsets: Array[(Int,Int)]) : (Int,Int) = {    
    val idx = lineOffsets.indexWhere( e => {
      e._1 <= absOffset && e._2 > absOffset
    })
    (idx,absOffset-lineOffsets(idx)._1)
  }
  
  
}