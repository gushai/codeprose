package org.codeprose.consumer.util

/**
 * Help in the handling of multiline comments.
 */
object CommentUtil {

  /**
   * Removes leading / * and  trailing * / and removes leading WS * WS in each lines (WS whitespace).
   * @param  multilineComment   Multiline comment. Either /* TEXT */ or /** TEXT */.
   * @return                    Text in multiline comment without the comment symbols.    
   */
  def cleanMultilineComment(multilineComment: String): String = {   
    val trimmed = multilineComment.trim()
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
   * @param   multilineComment  Multiline comment.
   * @return                    True if after trimming source begins with ScalaDoc comment begin / * *
   */
  def isScalaDocComment(multilineComment: String) : Boolean  = {
    if (multilineComment.trim().startsWith("/**"))
      return true
    else 
      return false
  }
  
  
}