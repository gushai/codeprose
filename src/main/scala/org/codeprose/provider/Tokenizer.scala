package org.codeprose.provider

import java.io.File
import scalariform.lexer.Token
import scalariform.lexer.ScalaLexer
import com.typesafe.scalalogging.LazyLogging




trait Tokenizer {
 def tokenize(file: File) : List[Token]  
}


object Tokenizer extends Tokenizer with LazyLogging{
  def tokenize(file: File): List[Token] = {
    logger.info("Tokenizing: \t" + file)
    val content = scala.io.Source.fromFile(file.getAbsolutePath(), "utf-8").getLines.mkString("\n")
    ScalaLexer.rawTokenise(content)     
   }
}