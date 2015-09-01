package org.codeprose.api

import org.codeprose.util.DynamicPropertyMap


class Token(val offset: Int, val text: String) extends DynamicPropertyMap {
	val length = text.length
  val range = Range(offset,offset+length)
  def toPrettyString() : String = { s"""Token($text,$offset,$length) w/ prop: """ + toString() }
}