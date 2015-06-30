package org.codeprose.consumer.util



trait MarkdownLibFeatures {
  
  /**
   * Converts a markdown string into html string.
   * @param s String containing markdown.
   * @return String containing html code.
   */
  def apply(s: String) : String 
}

object MarkdownConverter extends MarkdownLibFeatures {
  def apply(s: String): String = {    
    import org.fusesource.scalamd.{Markdown => mdlib}
   
     return mdlib.apply(s);
     
   
  }
}

