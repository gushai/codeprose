package org.codeprose.api

import org.codeprose.util.DynamicPropertyMap

/**
 * Language independent token which is easily enriched with language specific
 * information via the DynamicPropertyMap. 
 *
 * @param offset  Token offset in a file.
 * @param text    Token text
 * 
 */
class Token(val offset: Int, val text: String) extends DynamicPropertyMap {
	val length = text.length
  val range = Range(offset,offset+length)
  def toPrettyString() : String = { s"""Token($text,$offset,$length) w/ prop: """ + toString() }
}