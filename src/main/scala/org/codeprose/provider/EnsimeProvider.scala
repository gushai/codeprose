package org.codeprose.provider

import org.ensime.client.Client
import java.io.File
import org.ensime.api.OffsetRange
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}
import com.typesafe.scalalogging.LazyLogging
import java.util.regex.Pattern.CIBackRef
import org.ensime.api.ConnectionInfo
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try
import java.util.concurrent.TimeoutException
import org.ensime.client.ClientContext
import org.ensime.api.TypeInfo
import org.ensime.api.BasicTypeInfo
import org.ensime.api.ArrowTypeInfo
import org.ensime.api.SymbolInfo
import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token
import org.ensime.api.SymbolDesignations
import org.ensime.api.BasicTypeInfo
import org.ensime.api.ImplicitInfo

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
		) extends ProviderContext {
  
  val timeout_ConnectionInfoReq = 200
  val timeout_SymbolInfoReq = 200
  val timeout_SymbolDesignationsReq = 500
  val timeout_ImplicitInfoReq = 200
}


    
class EnsimeProvider(implicit c: EnsimeProviderContext) extends TokenEnricher with LazyLogging {

	private val ensimeClient = new Client()(new ClientContext(c.host,c.port,false)) 
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
			logger.info("[Ensime Server Connection]\t Testing connection to ensime-server ...")
			val connectionInfo = ensimeClient.connectionInfo()
			val serverReady = try {
				val cIResult= Await.result(connectionInfo,  Duration(c.timeout_ConnectionInfoReq, MILLISECONDS))
						logger.info("[Ensime Server Connection]\t Successfully completed." + "Server details:" + cIResult)
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


	def getEnrichedTokens(file: File) : scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token] = {
    if(isInitialized){			
      if(c.verbose)
				logger.info("\n\nProcessing: \t" + file)

      val rawTokens = getTokens(file)  
      val rawTokensWithSymbolDesignations = getSymbolDesignations(file,rawTokens)

      val tokens = rawTokens.map{t => enrichToken(file,t)}
      
 
      tokens
    } else {
      logger.error("Not initialized correctly.")
      scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token]()
    }
	}

  
  
  
  private def getSymbolDesignations(file: File, tokens: ArrayBuffer[Token]) : ArrayBuffer[Token] = {
    val end = tokens.last.offset + tokens.last.length
    val symbolDesignations = ensimeClient.symbolDesignations(file, 0, end, org.ensime.api.SourceSymbol.allSymbols) 
    
    try {
    	val cIResult= Await.result(symbolDesignations,  Duration(c.timeout_SymbolDesignationsReq, MILLISECONDS))
    }  catch {
    case timeout : TimeoutException => { 
    	logger.error("SymbolDesignationsReq:\t" + timeout.getMessage)     	
      }
 
    }

    
    symbolDesignations.onSuccess({
      
      case symDes : SymbolDesignations => {
        logger.info("Adding SymbolDesignations ... ")
        enrichTokensWithSymbolDesignations(tokens,symDes)
      }
      
    })
    
     symbolDesignations.onFailure({
        case t => {(logger.error("SymbolDesignationsReq failed! " + t))}
      })
    
    tokens
  } 
  
  private def getImplicitInformation(file: File, start: Int, end: Int) : Unit = {
    val implicitInfo = ensimeClient.implicitInfoReq(file,OffsetRange(start,end))
    
    try {
      val resultImplicitInfo = Await.result(implicitInfo,  Duration(c.timeout_ImplicitInfoReq, MILLISECONDS))
    }  catch {
    case timeout : TimeoutException => { 
      logger.error("ImplicitInfoReq:\t" + timeout.getMessage)       
      }
 
    }
    
    implicitInfo.onSuccess({
      
      case implInfo : ImplicitInfo => {
        logger.info("Adding ImplicitInformation ... ")
        println(implInfo)
        //enrichTokensWithSymbolDesignations(tokens,symDes)
      }
      
 
      
    })
    
     implicitInfo.onFailure({
        case t => {(logger.error("ImplicitInfoReq failed! " + t))}
      })
    
    
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
				    case Tokens.VARID => { enrichToken_VARID(file,token) 
          }
				  case _ => { 
            if(c.verbose){ 
              //logger.info("No information requested for " + tt) 
            } 
          }					
				} 
			}
			case _ => { 
				  logger.error("Oops: Not able to determine the token type of " + token.toPrettyString())
			  }
			} 

      token
      
	}

  private def enrichToken_VARID(file: File, token: org.codeprose.api.Token) : org.codeprose.api.Token = {
      import org.codeprose.api.ScalaLang._      
      import org.codeprose.api.TokenProperties.SourcePosition    
      //println(file + " - " + OffsetRange(token.offset) + token.text)
     
        
      // SymbolInfoReq
      val symbolInfo = ensimeClient.symbolAtPoint(file, token.offset)
      
      
      // Awaiting the symbolInfo
      try {
        val cIResult= Await.result(symbolInfo,  Duration(c.timeout_SymbolInfoReq, MILLISECONDS))
      } catch {
        case timeout : TimeoutException => {
          logger.error("[RequestError] \tSymbolInfosReq:\t" + timeout.getMessage)       
        }
     
      }
     // 
      
      
      symbolInfo.onSuccess({
        case sI: SymbolInfo => {
          
         
          val typeInfo = sI.tpe
          if(typeInfo.outerTypeId.isDefined)
            token.set(outerTypeId)(typeInfo.outerTypeId.get)
          
          token.set(fullName)(typeInfo.fullName)
          
          token.set(typeId)(typeInfo.typeId)
          token.set(declaredAs)(typeInfo.declAs.toString)
          
          if(typeInfo.args.size>0)                      
            token.set(args)(typeInfo.args.mkString(","))
          if(typeInfo.members.size >0)  
            token.set(members)(typeInfo.members.mkString(","))
          if(typeInfo.typeArgs.size > 0)
            token.set(typeArgs)(typeInfo.typeArgs.mkString(","))
                    
          // TypeInfo
          if(typeInfo.isInstanceOf[BasicTypeInfo]){    
            token.set(isArrowType)(false)
          } else if (typeInfo.isInstanceOf[ArrowTypeInfo]){        
            token.set(isArrowType)(false)
          }
          
          if(sI.declPos.isDefined && sI.declPos.get.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
                    token.set(declaredAt)(
                        new SourcePosition(sI.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath,
                        sI.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].offset))
            
          }
        }
      })
      
       symbolInfo.onFailure({
        case t => {
          (logger.error("[RequestError] \tSymbolInfoReq failed! " + t))
          }
      })
      
      token
  }
  
  
	private def getTokens(file: java.io.File) : ArrayBuffer[Token]= {
		
    logger.info("Getting raw tokens.")
		import org.codeprose.util.FileUtil    
		val srcContent = FileUtil.loadSrcFileContent(file)        
		val tokens = org.codeprose.provider.ScalaTokenizer.tokenize(srcContent)
		tokens
	}
  
  
  
  private def enrichTokensWithSymbolDesignations(
      tokens: ArrayBuffer[Token], 
      symDesignations: SymbolDesignations) : ArrayBuffer[Token] = {
    
    import org.codeprose.api.ScalaLang._
    
    symDesignations.syms.foreach { symDes => {
      val idx = tokens.indexWhere({ t =>
         val idx = t.offset+t.length/2
         symDes.start <= idx && symDes.end > idx
        },0)
        if(idx != -1){
          tokens(idx).set(symbolDesignation)(SourceSymbol.mapEnsimeToCodeprose(symDes.symType))
        }
      } 
    }    
    tokens    
  }
  
}