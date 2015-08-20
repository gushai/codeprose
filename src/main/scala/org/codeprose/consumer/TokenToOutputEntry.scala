package org.codeprose.consumer

import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token






/**
 * Element to map from token to output entry.
 * @param token Token
 * @return      
 */
trait TokenToOutputEntry {
  val scriptElements = ArrayBuffer[String]()
 // def getTokenEntry(token: Token) : (String,String)
   def getTokenEntry(token: Token) : String
  
}

//class TokenToOutputEntryHtml(val filenamesOriginalToOutputNames: Array[(String,String)]) extends TokenToOutputEntry {
class TokenToOutputEntryHtml() extends TokenToOutputEntry { 
  def getTokenEntry(token: Token) : String = {
    token.text
  }  
}
