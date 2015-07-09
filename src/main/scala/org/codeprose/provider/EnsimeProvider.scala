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
import scala.util.Try
import java.util.concurrent.TimeoutException

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
	var isInitialized = false
  
	/*
	 * Initializes the ensime client and tests the connection to the server.
	 * Sets isInitialized. 
	 */
	def initialize(): Unit = {
			logger.info("Initializing Ensime client ... ")    
			ensimeClient.initialize()         
			logger.info("Done.")

			isInitialized = testConnection()       
	}

	/*
	 * Send a ConnectionInfo to the server and returns boolean success indicator.
	 * Blocks during test.
	 */
	def testConnection() : Boolean = {
			logger.info("Testing connection to ensime-server ...")
			val connectionInfo = ensimeClient.connectionInfo()
			val serverReady = try {
				val cIResult= Await.result(connectionInfo,  Duration(1500, MILLISECONDS))
						logger.info("Successfully completed.")
						true
			}  catch {
			case timeout : TimeoutException => { 
				logger.error("Failed:\t" + timeout.getMessage) 
				false
			}
			}
			serverReady
	}

	def close() : Unit = {
			ensimeClient.close()
	}

	def shutdownServer() : Unit = {
			if(c.verbose)
				logger.info("Shutting down ensime-server...")
			ensimeClient.shutdownServer()
				if(c.verbose)
					logger.info("Done.")
	}

	def getEnrichedTokens(file: File) : scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token] = {
    if(isInitialized){			
      if(c.verbose)
				logger.info("Processing: \t" + file)

      val tokens = getTokens(file).map{t => enrichToken(file,t)}
      
      // TODO: Professional waiting for all futures. Set limit of 3sec??
      logger.info("Awaiting results of token enrichment...")
      Thread.sleep(5000)    
            
      tokens
    } else {
      logger.error("Not initialized correctly.")
      scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token]()
    }
	}

  /*
   * Enriches the token with additional information obtained from ensime-server.
   */
	private def enrichToken(file: File, token: org.codeprose.api.Token) : org.codeprose.api.Token = {
		  
      import org.codeprose.api.ScalaLang._      
      
			val tokenTyp = token(tokenType)

			tokenTyp match {
			  case Some(tt) => {
				  tt match {       
				    case Tokens.VARID => { enrichToken_VARID(file,token) }
				  case _ => { if(c.verbose){ logger.info("No information requested for " + tt) } }					
				} 
			}
			case _ => { 
				  logger.error("Oops: Not able to determine the token type of " + token.toPrettyString())
				  //throw new Exception("Unknown to determine the token type! Token: " + token.toPrettyString())
			  }
			} 

      token
      
	}

  private def enrichToken_VARID(file: File, token: org.codeprose.api.Token) : org.codeprose.api.Token = {
      import org.codeprose.api.ScalaLang._      
      import org.codeprose.api.TokenProperties.SourcePosition    
    
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
      token
  }
  
  
	private def getTokens(file: java.io.File) = {
		logger.info("Getting raw tokens.")
		import org.codeprose.util.FileUtil    
		val srcContent = FileUtil.loadSrcFileContent(file)        
		val tokens = org.codeprose.provider.ScalaTokenizer.tokenize(srcContent)
		tokens
	}
}