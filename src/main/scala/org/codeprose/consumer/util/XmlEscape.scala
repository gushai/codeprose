package org.codeprose.consumer.util

/**
 * Escapes xml.
 * 
 * Uses scala.xml.Utility.escape()
 * 
 * @param text  String
 * @return      String characters < > & and " from string escaped from text. 
 */
trait XmlEscape {
  def escape(text: String) : String = {
    xml.Utility.escape(text)
  }
}

object XmlEscape extends XmlEscape