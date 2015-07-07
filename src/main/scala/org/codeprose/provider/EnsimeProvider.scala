package org.codeprose.provider

import org.ensime.client.Client
import java.io.File
import org.ensime.model.OffsetRange
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}
import com.typesafe.scalalogging.LazyLogging
import java.util.regex.Pattern.CIBackRef
import org.ensime.server.ConnectionInfo
import scala.concurrent.Await
import scala.concurrent.duration._

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

trait ProviderContext { val verbose: Boolean }


class EnsimeProviderContext(
    val host: String,
    val port: Int,
    val verbose: Boolean
    ) extends ProviderContext

class EnsimeProvider(implicit c: EnsimeProviderContext) extends TokenEnricher with LazyLogging {

  private val ensimeClient = new Client(c.host,c.port)
  ensimeClient.initialize()
  var isInitialized = false
  
  def shutdownServer() : Unit = {
    ensimeClient.shutdownServer()
  }
 
  
  def initialize(): Unit = {
//    logger.info("Initializing Ensime client ... ")    
//    ensimeClient.initialize()         
//    logger.info("Done.")
//    
//    isInitialized = testConnection()
//        
}

  /*
   * Send a ConnectionInfo to the server and returna boolean sucess indicator.
   */
  def testConnection() : Boolean = {
    /// TODO: 
    // Get connection info and initialize project
    // Requires a more sophisticated version of the ensimeclient to return information on messages that have no return point
    // TODO: Check if certain messages need to be send to initialize project
    val serverReady = false
//    val serverReady : Future[Boolean] 
//
//    logger.info("Testing connection ...")
//    val connectionInfo = ensimeClient.connectionInfo()
//    connectionInfo onSuccess({
//      case cI : ConnectionInfo => {
//          logger.info(cI.toString)
//          logger.info("Connection successfully tested")
//          serverReady = true          
//        }         
//      })
//    
//    connectionInfo onFailure({
//      case _ => {          
//         logger.error("Connection failed!")
//         serverReady = false
//      }
//     })
//    
//    Await.result(connectionInfo, 3.seconds )
    
    serverReady
  }
  
  def close() : Unit = {
    ensimeClient.close()
  }

  def getEnrichedTokens(file: File) : scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token] = {
    if(c.verbose)
      logger.info("Processing: \t" + file)
    
    val tokens = getTokens(file)
    
    
    import org.codeprose.api.ScalaLang._
    import org.codeprose.api.ScalaTokens._
    import org.codeprose.api.TokenProperties.SourcePosition
    
    for (token <- tokens){
     
      val tokenTyp = token(tokenType)
      
    		  tokenTyp match {
    		  case Some(tt) => {
    			  tt match {

    			  case VARID => {             	 
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
              case _ => { 
                if(c.verbose)
                  logger.info("No information requested for " + tt)
               
              }

    			  } 

    		  }
          case _ => { 
            logger.error("Oops: Not able to determine the token type of " + token.toPrettyString())
            throw new Exception("Unknown to determine the token type! Token: " + token.toPrettyString())
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
}