package org.codeprose.consumer.util


/**
 * Markdown converter.
 */
trait MarkdownLibFeatures {

	/**
	 * Converts a markdown string into html string.
	 * @param   mdText  Text containing markdown.
	 * @return          Text with markdown converted to html.
	 */
	def apply(mdText: String) : String 
}


object MarkdownConverter extends MarkdownLibFeatures {
  
	/**
	 * Converts a markdown string into html string.
   * 
   * Uses: org.fusesource.scalamd.Markdown
   * 
	 * @param   mdText  Text containing markdown.
	 * @return          Text with markdown converted to html.
	 */
	def apply(mdText: String): String = {    
			import org.fusesource.scalamd.{Markdown => mdlib}
			return mdlib.apply(mdText);
	}
}

