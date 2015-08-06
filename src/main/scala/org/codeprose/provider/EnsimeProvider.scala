package org.codeprose.provider

import org.ensime.client.Client
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}
import com.typesafe.scalalogging.LazyLogging
import java.util.regex.Pattern.CIBackRef
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try
import java.util.concurrent.TimeoutException
import org.ensime.client.ClientContext
import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token
import org.ensime.api._
import org.codeprose.api.TokenProperties.SourcePosition


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
		val verbose: Boolean,
    val inputFolders: List[String]
		) extends ProviderContext {
  
  val timeout_ConnectionInfoReq = 200
  val timeout_SymbolInfoReq = 200
  val timeout_SymbolDesignationsReq = 500
  val timeout_ImplicitInfoReq = 200
  val timeout_UsesOfSymbolAtPointReq = 200
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
						logger.info("[Ensime Server Connection]\t Successfully completed. " + "Server details: " + cIResult)
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
				logger.info("\n\nProcessing: \t" + file + "\n======================================================")

      val rawTokens = getTokens(file)  
      val rawTokensWithSymbolDesignations = getSymbolDesignations(file,rawTokens)
      val rawTokensWithImplicitInformation = getImplicitInformation(file,rawTokensWithSymbolDesignations)
      
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
  
  private def getImplicitInformation(file: File, tokens: ArrayBuffer[Token]) : ArrayBuffer[Token] = {
    
    logger.info("Adding ImplicitInformation ... ")
    
    val end = tokens.last.offset + tokens.last.length
    val implicitInfo = ensimeClient.implicitInfoReq(file,OffsetRange(0,end))
    
    try {
      val resultImplicitInfo = Await.result(implicitInfo,  Duration(c.timeout_ImplicitInfoReq, MILLISECONDS))
    }  catch {
    case timeout : TimeoutException => { 
      logger.error("ImplicitInfoReq:\t" + timeout.getMessage)       
      }
 
    }
    
    implicitInfo.onSuccess({
      
      case implInfo : ImplicitInfos => {
         enrichTokensWithImplicitInformation(tokens, implInfo)
       }
      case _ => {
          (logger.error("ImplicitInfoReq failed! "))
        }
      
    })
    
     implicitInfo.onFailure({
        case t => {(logger.error("ImplicitInfoReq failed! " + t))}
      })
    
    tokens
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
      
      // ==================================
      // Uses of Symbol
      
      
      val usesOfSymbol = ensimeClient.usesOfSymAtPoint(file, token.offset)
      
      
      // Awaiting the symbolInfo
      try {
        val cIResult= Await.result(usesOfSymbol,  Duration(c.timeout_SymbolInfoReq, MILLISECONDS))
      } catch {
        case timeout : TimeoutException => {
          logger.error("[RequestError] \tUsesOfSymbolAtPointReq:\t" + timeout.getMessage)       
        }
     
      }
      
      usesOfSymbol.onSuccess({
        case uOS: ERangePositions => {
          
          // Filters the references that are not in the input folders!
          val posToSave = uOS.positions.filter(srcPos =>
            c.inputFolders.map( folder => srcPos.file.startsWith(folder)).reduce(_ || _)            
            )
          if(posToSave.size > 0){
          token.set(whereUsed)(posToSave.map({
            srcPos => 
                new org.codeprose.api.TokenProperties.ERangePosition(
                srcPos.file,
                srcPos.offset,
                srcPos.start,
                srcPos.end
                )}
            )
            )  
          }   
        } 
      })
      
       usesOfSymbol.onFailure({
        case t => {
          (logger.error("[RequestError] \tUsesOfSymbolAtPointReq failed! " + t))
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
  
  private def enrichTokensWithImplicitInformation(
      tokens: ArrayBuffer[Token], 
      implicitInfos: ImplicitInfos) : ArrayBuffer[Token] = {
    
    import org.codeprose.api.ScalaLang._
    
    implicitInfos.infos.foreach( { implicitInfo => {
      
      implicitInfo match {
         case info : ImplicitConversionInfo => {
           // Affected token
           val idx = tokens.indexWhere({ t =>
             val idx = t.offset+t.length/2
             info.start <= idx && info.end > idx
            },0)
            
           // Save information
          if(idx != -1){
            
            tokens(idx).set(implicitConversion_fullName)(info.fun.name)
            if(info.fun.declPos.isDefined && info.fun.declPos.get.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
             tokens(idx).set(implicitConversion_sourcePosition)( new SourcePosition(
                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath,
                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].offset)
             )
            }
          } else {
            logger.error("[ImpicitConverionInfo]\t" + "Failed to determine affected token. " + info)
          }
        }
        case info : ImplicitParamInfo => {
          logger.error("ImplicitParamInfo: Not yet collected!")
        }
      }
      
      } 
    })  
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