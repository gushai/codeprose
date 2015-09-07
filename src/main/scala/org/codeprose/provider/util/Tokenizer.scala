package org.codeprose.provider.util


import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token

/**
 * Generates a list of Tokens from source code.
 * @param src Source code.
 * @return    Resulting codeprose tokens.
 */
trait Tokenizer {
  def tokenize(src: String) : ArrayBuffer[Token]
}

