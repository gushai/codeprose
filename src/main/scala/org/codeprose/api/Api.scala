package org.codeprose.api

import java.io.File
import scala.collection.mutable.ArrayBuffer
import org.codeprose.util.DynamicPropertyMap

/**
 * Class to be exchanged between Provider <-> Broker <-> Consumer
 * 
 * Use language specific key-value-pairs to enrich the tokens and save 
 * project summary information. 
 */
class ProjectInfo (  
  val enrichedTokens : ArrayBuffer[(java.io.File, ArrayBuffer[Token])],
  val summary : ProjectSummary
  ) {
}


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
	import DynamicPropertyMap._
	trait TokenType {}
  
  // Keys
  val tokenType = new Key('tokenType){ type Value <: TokenType }    
	
}



