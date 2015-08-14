package org.codeprose.provider

import org.ensime.client.Client
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}
import com.typesafe.scalalogging.LazyLogging
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try
import java.util.concurrent.TimeoutException
import org.ensime.client.ClientContext
import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token
import org.ensime.api._
import org.codeprose.api.TokenProperties._


trait TokenEnricher {
	def initialize() : Unit 
	def close() : Unit    
	//def getEnrichedTokens(file : File) : scala.collection.mutable.ArrayBuffer[org.codeprose.api.Token]
  def getEnrichedTokens(files : Array[File]) : 
  ArrayBuffer[(File,ArrayBuffer[org.codeprose.api.Token])]
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
  
  val timeout_ConnectionInfoReq = 500
  val timeout_SymbolInfoReq = 250
  val timeout_SymbolDesignationsReq =700
  val timeout_ImplicitInfoReq = 700
  val timeout_UsesOfSymbolAtPointReq = 700
  val timeout_InspectTypeByIdReq = 500
}


    
class EnsimeProvider(implicit c: EnsimeProviderContext )
    extends TokenEnricher with LazyLogging {

	private val ensimeClient = new Client()(new ClientContext(c.host,c.port,false)) 
	var isInitialized = false
  var tokenId = 0
  var occuringTypeIds = scala.collection.mutable.SortedSet[(Int,String)]()
 
  
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


	def getEnrichedTokens(files: Array[File]) : 
  ArrayBuffer[(File,ArrayBuffer[org.codeprose.api.Token])] = {
   
    val out = ArrayBuffer[(File,ArrayBuffer[org.codeprose.api.Token])]()
    if(isInitialized){			
      
      
     for(file <- files){
        
        logger.info("Getting tokens: ")
        if(c.verbose)
				  logger.info("\n\nProcessing: \t" + file + "\n======================================================")
     
        import org.codeprose.api.ScalaLang._
        val rawTokens = getTokens(file).map(t=>  {t.set(internalTokenId)(tokenId)
          tokenId += 1
          t
        }) 
        
      
        val rawTokensWithSymbolDesignations = getSymbolDesignations(file,rawTokens)
        val rawTokensWithImplicitInformation = getImplicitInformation(file,rawTokensWithSymbolDesignations)
        
        val tokens = rawTokensWithImplicitInformation.map{t => enrichToken(file,t)}
      
 
        out += ((file,tokens))
      }
     
     // Inspect types occuring in the project
     
     getDetailedTypeInformation()
     
     
     // Translation of sourcePostions file offset to file Token id
     logger.info("Updating declared at source positions ... ")
     includeTokenBasedSourcePostion_declaredAt(out)
     includeTokenBasedSourcePostion_whereUsedWithinFile(out)
     
     
    } else {
      logger.error("Not initialized correctly.")
    }
    out
	}

  
  private def getDetailedTypeInformation() : Unit = {
    
    println("Occuring Type Ids: [ALL] " + occuringTypeIds.map(e => e._1 + ": " + e._2).mkString("\n") + "\n")
    
    val occurIdFiltered = occuringTypeIds.filter(e => (e._2.contains("rational")))
    println("Occuring Type Ids: [RATIONAL]" + occurIdFiltered.map(e => e._1 + ": " + e._2).mkString("\n") + "\n")

    
//    occurIdFiltered.map(e=>e._1).map(typeId => {
//      (typeId,performInspectTypeByIdReq(typeId))
//      }).foreach(e=> { 
//        println(e._1 + ": " + e._2 +"\n")
//      }) 
    
      // 
      
      println("Individual Req: 1 [XXX]\n " + performInspectTypeByIdReq(7) + "\n\n")
      Thread.sleep(2000)
      println("Individual Req: 2 " +performInspectTypeByIdReq(7))
      Thread.sleep(2000)
      println("Individual Req: 3 " +performInspectTypeByIdReq(7))
  }
  
  private def performInspectTypeByIdReq(typeId: Int) : String = {
    val typeInspectInfo = ensimeClient.inspectTypeById(typeId) 
    
    try {
      val result = Await.result(typeInspectInfo,  Duration(c.timeout_InspectTypeByIdReq, MILLISECONDS))
      println("InspectTypeById: got result, companionId: " + result.companionId + " - numInterfaces: " + result.interfaces.size)
      
      println("===================== - Beg\n")
      println("compID: " + result.companionId)
      println("\n")
      println("typeInfo: " + result.`type`)
      println("\n")
      val interStr = result.supers.map( e => "TypeInfo: " + 
          "\tname: \t" + e.`type`.name + "\n" +
          "\ttypeid: \t" + e.`type`.typeId + "\n" +
          "\tdeclAs: \t" + e.`type`.declAs + "\n" +
          "\tfullName: \t" + e.`type`.fullName + "\n" +
          "\ttypeArgs: \t" + e.`type`.typeArgs + "\n" +
          "\tmembers: \t\n" + e.`type`.members.map(e => e.toString() +"\t\t").mkString("\n") + "\n" +
          "\tpos: \t" + e.`type`.pos + "\n" +
          "\touterTypeId: \t" + e.`type`.outerTypeId + "\n" +
          "\n\nViaView: " + e.viaView + "\n----------\n" )
      println("interfaces:\n" + interStr.mkString("\n"))
      
      println("\n===================== - End:\n")
    }  catch {
    case timeout : TimeoutException => { 
      logger.error("InspectTypeByIdReq:\t" + timeout.getMessage)       
      }
    }
    
    var s=""
    
    typeInspectInfo.onSuccess({
      
      case iTI : TypeInspectInfo => {
        val siz = iTI.supers.size
        val typ = iTI.`type`
        val inter = iTI.interfaces.toList
       // println(inter)
       // s = typ.toString()
        s=iTI.toString() + "\n__" + siz
      }
      
    })
    
     typeInspectInfo.onFailure({
        case t => {(logger.error("SymbolDesignationsReq failed! " + t))}
      })
      
     s
  }
  
  private def performInspectTypeAtPoint(file: File, range: OffsetRange) : String = {
//    
//    val typeInspectInfo = ensimeClient.inspectTypeAtPoint(file, range) 
//    
//    try {
//      val result = Await.result(typeInspectInfo,  Duration(c.timeout_InspectTypeByIdReq, MILLISECONDS))
//      
//      
//    }  catch {
//    case timeout : TimeoutException => { 
//      logger.error("InspectTypeByIdReq:\t" + timeout.getMessage)       
//      }
//    }
//    
//    var s=""
//    
//    typeInspectInfo.onSuccess({
//      
//      case iTI : TypeInspectInfo => {
//        println("InspectTypeById: got result, companionId: " + iTI.companionId + " - numInterfaces: " + iTI.interfaces.size)
//         s=iTI.toString()
//      }
//      
//    })
//    
//     typeInspectInfo.onFailure({
//        case t => {(logger.error("SymbolDesignationsReq failed! " + t))}
//      })
//      
//     s
    ???
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
          
          if(tt.isId){
            tt match {
             case Tokens.VARID => { 
               enrichToken_VARID(file,token)              
             } 
             case _ => {
               enrichToken_Id(file,token)
             }
            }
            
          } else {
          
				  tt match {       
				    case _ => { 
            if(c.verbose){ 
              //logger.info("No information requested for " + tt) 
            } 
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
          /* Inspect Type Stuff - Beg */
          
//          if(!occuringTypeIds.contains(typeInfo.typeId)){
//            println(typeInfo.typeId + ": " + performInspectTypeAtPoint(file, OffsetRange(token.offset,token.offset)))
//          }
          /* Inspect Type Stuff - End */
          
          occuringTypeIds += ((typeInfo.typeId,typeInfo.fullName))
          
          
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
            // Raw where used data (contains uses in other files)
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
  
  private def enrichToken_Id(file: File, token: org.codeprose.api.Token) : org.codeprose.api.Token = {
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
          occuringTypeIds += ((typeInfo.typeId,typeInfo.fullName))
          
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
            // Raw where used data (contains uses in other files)
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
            tokens(idx).set(implicitConversion_indicator)(true)
            tokens(idx).set(implicitConversion_fullName)(info.fun.name)
            if(info.fun.declPos.isDefined && info.fun.declPos.get.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
             tokens(idx).set(implicitConversion_sourcePosition)( new org.codeprose.api.TokenProperties.SourcePosition(
                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath,
                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].offset)
             )
            }
          } else {
            logger.error("[ImpicitConverionInfo]\t" + "Failed to determine affected token. " + info)
          }
        }
        case info : ImplicitParamInfo => {
          println("[--------------------------------------------]")
          println(info + "\n")
          println("start:\t\t" + info.start)
          println("end:\t\t" + info.end)
          println("fun:\t\t" + info.fun)
          println("params:\t\t " + info.params)
          println("funIsImplicit:\t\t" + info.funIsImplicit)
          println("[--------------------------------------------]\n")
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
 
  
  private def includeTokenBasedSourcePostion_declaredAt(info : ArrayBuffer[(File,ArrayBuffer[Token])]) : 
  ArrayBuffer[(File,ArrayBuffer[Token])] = {
    import org.codeprose.api.ScalaLang._
    
    
   info.map(e =>{
     val file = e._1
     val tokens = e._2

     
     tokens.map(t => {
       if(t(declaredAt).isDefined){
         val srcPos = t(declaredAt).get
         val tId = findInternalTokenIdToOffset(srcPos,info)
         if(tId != -1){
          t.set(declaredAt_TokenIdSrcPos)( new SourcePositionWithTokenId(srcPos.filename,tId))
         }
         t
       } else {
         t
       }
     })
     
     (file,tokens)
   })
        
  }

   private def includeTokenBasedSourcePostion_whereUsedWithinFile(info : ArrayBuffer[(File,ArrayBuffer[Token])]) : 
  ArrayBuffer[(File,ArrayBuffer[Token])] = {
    import org.codeprose.api.ScalaLang._
    
    
   info.map(e =>{
     val file = e._1
     val tokens = e._2

     
     tokens.map(t => {
       if(t(whereUsed).isDefined){
         
         val srcPosList = t(whereUsed).get
         val tIds = srcPosList.filter(srcPos => srcPos.filename == file.getAbsolutePath()).map( srcPos => findInternalTokenIdToOffset(srcPos,info))
         if(tIds.length >0){
           t.set(whereUsed_WithinFileTokenIdSrcPos)(tIds.map(id => new SourcePositionWithTokenId(file.getAbsolutePath,id)))
         }
         t
       } else {
         t
       }
     })
     
     (file,tokens)
   })
        
  }
  
  private def findInternalTokenIdToOffset(
      srcPos: org.codeprose.api.TokenProperties.SourcePosition, 
      info: ArrayBuffer[(File,ArrayBuffer[Token])]) : Int = {
	 
    import org.codeprose.api.ScalaLang._
  
    val tokens = info.filter({e => e._1.getAbsolutePath == srcPos.filename}).map(e=>e._2)
    
    val id = if(tokens.length==0){
      -1
    } else {
      
      val t = tokens(0).filter(t => {
        t.offset == srcPos.offset
      })
    
      val ret = if(t.length>0 && t(0)(internalTokenId).isDefined){
        t(0)(internalTokenId).get
      } else {
         logger.error("[findInternalTokenIdToOffset]\t" + "Could not determine internalTokenId." )
        -1
      }
      ret
    }
    id 
   
	}
  
   private def findInternalTokenIdToOffset(
      srcPos: org.codeprose.api.TokenProperties.ERangePosition, 
      info: ArrayBuffer[(File,ArrayBuffer[Token])]) : Int = {
   
    import org.codeprose.api.ScalaLang._
  
    val tokens = info.filter({e => e._1.getAbsolutePath == srcPos.filename}).map(e=>e._2)
    
    val id = if(tokens.length==0){
      -1
    } else {
      
      val t = tokens(0).filter(t => {
        t.offset == srcPos.offset
      })
    
      val ret = if(t.length>0 && t(0)(internalTokenId).isDefined){
        t(0)(internalTokenId).get
      } else {
         logger.error("[findInternalTokenIdToOffset]\t" + "Could not determine internalTokenId." )
        -1
      }
      ret
    }
    id 
   
  }
  
}