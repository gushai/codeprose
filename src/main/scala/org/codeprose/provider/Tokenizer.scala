package org.codeprose.provider

import java.io.File
import scalariform.lexer.Token
import scalariform.lexer.ScalaLexer




trait Tokenizer {
 def tokenize(file: File) : List[Token]  
}


object Tokenizer extends Tokenizer {
  def tokenize(file: File): List[Token] = {
    val content = scala.io.Source.fromFile(file.getAbsolutePath(), "utf-8").getLines.mkString("\n")
    ScalaLexer.rawTokenise(content)     
   }
}