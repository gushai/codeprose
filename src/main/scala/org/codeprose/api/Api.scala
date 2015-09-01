package org.codeprose.api

import java.io.File
import scala.collection.mutable.ArrayBuffer
import org.codeprose.util.DynamicPropertyMap


class ProjectInfo (  
  val enrichedTokens : ArrayBuffer[(java.io.File, ArrayBuffer[Token])],
  val summary : ProjectSummary
  ) {
}


/**
 * 
 */
trait DefaultLang {
	import DynamicPropertyMap._
	trait TokenType {}
  
  // Keys
  val tokenType = new Key('tokenType){ type Value <: TokenType }    
	
}



