package org.codeprose.consumer.util

/**
 * Escapes xml.
 * 
 * Uses scala.xml.Utility.escape()
 */
trait XmlEscape {
  /**
   * Escapes xml.
   * @param   text  String
   * @return        String with characters < > & and " escaped. 
   */
  def escape(text: String) : String = {
    xml.Utility.escape(text)
  }
}

object XmlEscape extends XmlEscape