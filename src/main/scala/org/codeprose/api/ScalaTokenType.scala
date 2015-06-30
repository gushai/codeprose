package org.codeprose.api

trait TokenType {
  
}

case class ScalaTokenType(name: String, isXml: Boolean = false) {

  import org.codeprose.api.{ScalaTokens => Tokens}
  
  def isNewline = this == Tokens.NEWLINE || this == Tokens.NEWLINES

  def isKeyword = Tokens.KEYWORDS contains this

  def isComment = Tokens.COMMENTS contains this

  def isId = Tokens.IDS contains this

  def isLiteral = Tokens.LITERALS contains this

  override lazy val toString = name

}