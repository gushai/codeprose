package org.codeprose.api

trait TokenType {
  val name : String 
}

case class ScalaTokenType(val name: String, isXml: Boolean = false) extends TokenType {

  import org.codeprose.api.{ScalaTokens => Tokens}
  
  def isNewline = this == Tokens.NEWLINE || this == Tokens.NEWLINES

  def isKeyword = Tokens.KEYWORDS contains this

  def isComment = Tokens.COMMENTS contains this

  def isId = Tokens.IDS contains this

  def isLiteral = Tokens.LITERALS contains this

  override lazy val toString = name

}