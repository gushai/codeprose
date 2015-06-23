package org.codeprose.consumer

trait MarkDownFeatures {
  def apply(source: String) : String  
}

object MarkDownScalaMd extends MarkDownFeatures {
  
  def apply(source: String): String = {
    import org.fusesource.scalamd.Markdown
    Markdown.apply(source)
  }    
 
}