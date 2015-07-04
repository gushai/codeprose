package org.codeprose.provider

import org.ensime.client.Client
import java.io.File
import org.ensime.model.OffsetRange
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}
import com.typesafe.scalalogging.LazyLogging

trait TokenEnricher {
   def initialize() : Unit 
   def close() : Unit    
   def getEnrichedTokens(file : File) : scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token]
}

// TODO: Do clearer specification of meta information specification
//trait MetaInformationEnricher {
//   def initializeMeta() : Unit 
//   def close() : Unit   
//} 
   

class EnsimeProvider(host: String, port: Int) extends TokenEnricher with LazyLogging {

  private val ensimeClient = new Client(host,port)
  ensimeClient.initialize()
  var isInitialized = false
  
  def shutdownServer() : Unit = {
    ensimeClient.shutdownServer()
  }
 
  
  def initialize(): Unit = {
    logger.info("Initializing Ensime client ... ")
    ensimeClient.initialize()
    logger.info("Done.")
    /// TODO: 
    // Get connection info and initialize project
    // Requires a more sophisticated verion of the ensimeclient to return information on messages that have no return point
    
    val connectionInto = ensimeClient.connectionInfo()
    logger.info("Connection Info: " + connectionInto.toString())
    
    // TODO: Check if certain messages need to be send to initialize project
    // send
    //Init project message
    isInitialized = true
  }
  
  def close() : Unit = {
    ensimeClient.close()
  }

  def getEnrichedTokens(file: File) : scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token] = {
  
    logger.info("Processing: \t" + file)
    val tokens = getTokens(file)
    logger.info("Enriching tokens.")
    
    import org.codeprose.api.ScalaLang._
    import org.codeprose.api.ScalaTokens
    import org.codeprose.api.TokenProperties.SourcePosition
    for (token <- tokens){
     
      token(tokenType) match {
        
        case ScalaTokens.VARID => {             	 
    	    val typeInfo = ensimeClient.typeAtPoint(file, OffsetRange(token.offset))
        
    	  typeInfo onSuccess({
    	  case Some(tI) => {
          if(!tI.pos.isDefined)
    		  {
            token.set(fullName)(tI.fullName)
            token.set(typeId)(tI.typeId)
            token.set(declaredAs)(tI.declAs.toString)
          }
          else{
            token.set(fullName)(tI.fullName)
            token.set(typeId)(tI.typeId)
            token.set(declaredAs)(tI.declAs.toString)
            token.set(declaredAt)(new SourcePosition(tI.pos.get.asInstanceOf[org.ensime.model.OffsetSourcePosition].file.getAbsolutePath,
                                  tI.pos.get.asInstanceOf[org.ensime.model.OffsetSourcePosition].offset))
          }
    	  }    	  
      })
        }
    	  
      } 
      
    }

    // TODO: Professional waiting for all futures. Set limit of 3sec??
    logger.info("Awaiting results of token enrichment...")
    Thread.sleep(4000)    
    return tokens
  }

  private def getTokens(file: java.io.File) = {
    logger.info("Getting raw tokens.")
    import org.codeprose.util.FileUtil    
    val srcContent = FileUtil.loadSrcFileContent(file)        
    val tokens = org.codeprose.provider.ScalaTokenizer.tokenize(srcContent)
    tokens
  }
  
  def getConnectionInfo() : Unit = {
    val f = ensimeClient.connectionInfo()       
    f onComplete {
      case Success(c) => {println(c.toString())}
      case Failure(c) => {println("Oops: Connection info failed!")}      
    }          
  }
}