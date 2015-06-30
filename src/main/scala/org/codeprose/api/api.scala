package org.codeprose.api

trait Language {

	trait DefaultToken {
		val offset: Int
		val text: String
		val length = text.length
	}

	type Token <: DefaultToken 

}

trait ScalaLang extends Language {

	trait ScalaToken extends DefaultToken {
		type tokenType <: ScalaTokenType
		val tokenType : ScalaTokenType
	}

	type TokenType <: ScalaToken 

case class PACKAGE(val offset: Int, val text: String) extends ScalaToken {
		val tokenType = ScalaTokenType("PACKAGE")
	}

case class STAR(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("STAR")
}

case class WHILE(val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("WHILE") }

case class CASE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("CASE")
}
case class NEW (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("NEW")
}
case class DO (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("DO")
}
case class EQUALS (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("EQUALS")
}
case class SUBTYPE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SUBTYPE")
}
case class EOF (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("EOF")
}
case class SEALED (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("SEALED")
}
case class TYPE (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("TYPE")
}
case class LBRACKET (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("LBRACKET")
}
case class FINAL (val offset: Int, val text: String) extends ScalaToken {
	val tokenType = ScalaTokenType("FINAL") 
}

}

trait TokenPropertyMapEntry {
    val key : Symbol
    type valueType <: Any    
    val valueType: valueType
}

trait OffsetEntry extends TokenPropertyMapEntry {
  val key = 'offset
  override type valueType = Int
}

  









