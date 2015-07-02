package org.codeprose.consumer.util

object CommentUtil {

  /**
   * Removes leading / * and  trailing * / and removes leading WS * WS in each lines (WS whitespace).
   * @param source multi line comment. Either /* */ or /** */
   * @return source string without the comment symbols    
   */
  def cleanMultilineComment(source: String): String = {   
    // Remove 
    val trimmed = source.trim()
    val splitted = if(trimmed.startsWith("/**")){
       "   " + trimmed.substring(3,trimmed.length-2) + "  "
    }      
    else {
      "  " + trimmed.substring(2,trimmed.length-2) + "  "
      }
    
    val rawlines = splitted.split("\n")
    return rawlines.map{ s => { 
        val idx = s.indexOf("*")
        if(idx>=0){
          //s.substring(0, idx) + " " + s.substring(idx+1,s.length) //s.slice(idx+1,s.length)
          (s.substring(0, idx) + " " + s.substring(idx+1,s.length)).trim() 
        }
        else{
           s
        }
      }
    }.mkString("\n")        
  }
  
  /**
   * Returns true if string begins with WS / * *
   * @param source multi line comment.
   * @return true if after trimming source begins with ScalaDoc comment begin / * *
   */
  def isScalaDocComment(source: String) : Boolean  = {
    if (source.trim().startsWith("/**"))
      return true
    else 
      return false
  }
  
  
}