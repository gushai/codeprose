package org.codeprose.provider.util


import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token


/**
 * Tokenizer to org.codeproseapi.Token
 */
trait Tokenizer {
/**
 * Generates Tokens from source code.
 * @param src Source code.
 * @return    Resulting codeprose tokens.
 */
  def tokenize(src: String) : ArrayBuffer[Token]
}

