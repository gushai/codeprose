package org.codeprose.api

import java.io.File
import scala.collection.mutable.ArrayBuffer
import org.codeprose.util.DynamicPropertyMap._

/**
 * Class to be exchanged between Provider <-> Broker <-> Consumer
 * 
 * Use language specific key-value-pairs to enrich the tokens and save 
 * project summary information. 
 */
case class ProjectInfo (  
  enrichedTokens : ArrayBuffer[(java.io.File, ArrayBuffer[Token])],
  summary : ProjectSummary)


/**
 * Basis for languages to to supported by codeprose.
 * 
 * Use to include: 
 *  - Token keys
 *  - Summary keys
 *  - Token types
 *  - Source symbols
 *  - ...
 *  
 *  
 *  See ScalaLang as an example.
 */
trait DefaultLang {
	
  /*
   * Token type to be implemented in the deriving language specifications. 
   */
	trait TokenType {}
  
  // Keys
  
  /*
   * Key for the token type of the specified language. Token types need to be implemented 
   * inheriting from the above TokenType trait.
   * 
   */
  val tokenType = new Key('tokenType){ type Value <: TokenType }    
	
}



