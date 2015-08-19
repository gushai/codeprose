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
import org.codeprose.api._


import org.ensime.api._


trait Provider {
	def initialize() : Unit 
	def close() : Unit      
  def getProjectInformation(files: List[File]) : ProjectInfo 
}


trait ProviderContext { val verbose: Boolean }

class EnsimeProviderContext(
		val host: String,
		val port: Int,
		val verbose: Boolean,
    val inputFolders: List[String]
		) extends ProviderContext {
  
  // All below in ms
  val timeout_ConnectionInfoReq = 500
  val timeout_SymbolInfoReq = 500
  val timeout_SymbolDesignationsReq = 700
  val timeout_ImplicitInfoReq = 700
  val timeout_UsesOfSymbolAtPointReq = 700
  val timeout_InspectTypeByIdReq = 500
  
  val pauseBetweenReq_InspectTypeById = 250
}


    
class EnsimeProvider(implicit c: EnsimeProviderContext )
    extends Provider with OccuringTypes with LazyLogging {

	private val ensimeClient = new Client()(new ClientContext(c.host,c.port,false)) 
	var isInitialized = false
  var tokenId = 0
 
 
  
	/**
	 * Initializes the ensime client and tests the connection to the server.
	 * Sets isInitialized to true if test successful. 
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

  /**
   * Generates project information. 
   * @param List of files to be processed. Assumes only Scala files are provided!
   * @return ProjectInfo
   */
   def getProjectInformation(files: List[File]) : ProjectInfo =  {
     
     // Enrich tokens
     val enrichedTokenPerFile = getEnrichedTokens(files)
     
     // Project summary information
     val summary = getProjectSummary(files)
          
     new ProjectInfo(enrichedTokenPerFile,summary)
   }
  
   
   /**
    * Creates project summary information. 
    */
  private def getProjectSummary(files: List[File]) : ProjectSummary = {
    
    val summary = new ProjectSummary()
    
    import org.codeprose.api.ScalaLang._
    
    // Files
    summary.set(fileList)(files)
    
    // Used types
    summary.set(typeInformation)(getDetailedTypeInformation())
    
    // Where used in project
    summary.set(whereUsedByTypeId)(getWhereUsedByTypeIdInformation())
   
    summary
  } 
   
  /**
   * Creates enriched Tokens.
   * @param Files to be processed. Assumes only Scala files are provided.
   * @return Enriched Tokens sorted per file. If isInitialized is false raw tokens are returned.
   */
	private def getEnrichedTokens(files: List[File]) : 
  ArrayBuffer[(File,ArrayBuffer[org.codeprose.api.Token])] = {
   
            
    val enrichedTokens = if(isInitialized){
    
      val rawTokens = getRawTokens(files: List[File])
      
      logger.info("Enriching tokens ...")
      logger.info("Adding type information ...")
      val tokens = rawTokens.map( e => {
        val et = e._2.map( t => { enrichToken(t,e._1,rawTokens)}) 
        (e._1,et)
       }) 
        
       
      logger.info("Adding semantic highlighting information")
      val tokenWithSemantic = tokens.map(e => {
        (e._1,enrichTokensWithSymbolDesignations(e._1,e._2))
      })
      
      logger.info("Adding implicit information ...")
      val tokenWithSemanticImplicit = tokenWithSemantic.map( e => {
        (e._1,enrichTokensWithImplicitInformation(e._1, e._2))
      })
      
      logger.info("Done.")
      
     // println(tokens.last._2.map(t=>t.toPrettyString()).mkString("\n"))
      tokens
    } else {
      logger.error("Not initialized correctly. Raw Tokens returned.")
      getRawTokens(files: List[File])
    }
    enrichedTokens 
  }
    
    
//    if(isInitialized){			
//      
//      
////     for(file <- files){
////        
////        logger.info("Getting tokens: ")
////        if(c.verbose)
////				  logger.info("\n\nProcessing: \t" + file + "\n======================================================")
////     
////        import org.codeprose.api.ScalaLang._
////        val rawTokens = getTokens(file).map(t=>  {t.set(internalTokenId)(tokenId)
////          tokenId += 1
////          t
////        }) 
////        
////      
////        val rawTokensWithSymbolDesignations = getSymbolDesignations(file,rawTokens)
////        val rawTokensWithImplicitInformation = getImplicitInformation(file,rawTokensWithSymbolDesignations)
////        
////        val tokens = rawTokensWithImplicitInformation.map{t => enrichToken(file,t)}
////      
//// 
////        out += ((file,tokens))
////      }
////     
////     
////     // Translation of sourcePostions file offset to file Token id
////     logger.info("Updating declared at source positions ... ")
////     includeTokenBasedSourcePostion_declaredAt(out)
////     includeTokenBasedSourcePostion_whereUsedWithinFile(out)
////     
////     
////    } else {
////      logger.error("Not initialized correctly.")
////    }
////    out
//	}

  /**
   * Generates the raw tokens.
   * @param List of files to process
   * @return Collection of enriched tokens per file. Each Token assigned a unique internalTokenId.
   */
  private def getRawTokens(files: List[File]) : 
  ArrayBuffer[(File,ArrayBuffer[Token])] = {
    
    import org.codeprose.util.FileUtil
    import org.codeprose.api.ScalaLang._
    
    logger.info("Generating raw tokens.")
    val tokensPerFile = ArrayBuffer[(File,ArrayBuffer[Token])]()
        
    
    for(file <- files){
      // TODO: Add try catch
      val srcContent =  FileUtil.loadSrcFileContent(file)         
      
      val tokens = org.codeprose.provider.ScalaTokenizer.tokenize(srcContent)
      val tokensWithInternalId = tokens.map(t => {
        tokenId+=1
        import org.codeprose.api.ScalaLang._
        t.set(internalTokenId)(tokenId)
        t
      })
      
      tokensPerFile.append( (file,tokensWithInternalId) )
    }
    tokensPerFile
  }
  
  /**
   * Enriches tokens with information that is obtained specifically for them.
   * @param Token to be enriched
   * @return Enriched token.
   * 
   * Note: Enrichment depends on the token type. Many tokens are not altered. 
   */
  private def enrichToken(
      token: Token, file: File, info: ArrayBuffer[(File,ArrayBuffer[Token])]) : Token = {
    
    import org.codeprose.api.ScalaLang._
    
    token(tokenType) match {
      case Some(tt) => {
        
        if(tt.isId){
          enrichTokenIds(token,file,info)
        } else {
          token  
        }
         
      }
      case None => {
        
      }
    }        
    token
  }

  
  private def enrichTokenIds(
      token: Token, file: File, info: ArrayBuffer[(File,ArrayBuffer[Token])]) : Token = {
        
    val enrichedTokenWithSymbol = enrichTokenWithSymbolInfo(token, file, info)
     
    enrichTokenWithUsesOfSymbolInfo(enrichedTokenWithSymbol, file, info)
  }
  
  private def enrichTokenWithSymbolInfo(
      token: Token, file: File, info: ArrayBuffer[(File,ArrayBuffer[Token])]) : Token = {
    
    val symbolInfoOption = performSymbolInfoRequest(file, token.offset) 
    
    symbolInfoOption match {
      case Some(symbolInfo) => {
        
        // Collect occuring type information
        val typeId = symbolInfo.tpe.typeId
        val fullname = symbolInfo.tpe.fullName
        addOccuringType(typeId, fullname)
        
        // Save type information on token
      
//        case sI: SymbolInfo => {
//          
//         
//          val typeInfo = sI.tpe
//          if(typeInfo.outerTypeId.isDefined)
//            token.set(outerTypeId)(typeInfo.outerTypeId.get)
//          
//          token.set(fullName)(typeInfo.fullName)
//          
//          token.set(typeId)(typeInfo.typeId)
//          occuringTypeIds += ((typeInfo.typeId,typeInfo.fullName))
//          
//          token.set(declaredAs)(typeInfo.declAs.toString)
//          
//          if(typeInfo.args.size>0)                      
//            token.set(args)(typeInfo.args.mkString(","))
//          if(typeInfo.members.size >0)  
//            token.set(members)(typeInfo.members.mkString(","))
//          if(typeInfo.typeArgs.size > 0)
//            token.set(typeArgs)(typeInfo.typeArgs.mkString(","))
//                    
//          // TypeInfo
//          if(typeInfo.isInstanceOf[BasicTypeInfo]){    
//            token.set(isArrowType)(false)
//          } else if (typeInfo.isInstanceOf[ArrowTypeInfo]){        
//            token.set(isArrowType)(false)
//          }
//          
//          if(sI.declPos.isDefined && sI.declPos.get.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
//                    token.set(declaredAt)(
//                        new SourcePosition(sI.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath,
//                        sI.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].offset))
//            
//          }
//        }
        
        token
      }
      case None => { token }
    }
  }
  
  
  
  /**
   * Enriches tokens with where used information. Records information 'globally'.
   * @param   token   Token to enrich.
   * @param   file    File of token.
   * @param   info    
   * @return          Enriched token.
   */
  private def enrichTokenWithUsesOfSymbolInfo(
      token: Token, file: File, info: ArrayBuffer[(File,ArrayBuffer[Token])]) : Token = {
   
    val usesOfSymbolsOption = performUsesOfSymbolReq(file, token.offset)
    
    usesOfSymbolsOption match {
      case Some(sourcePositions) => {
//        println("[UsesOfSymbolAtPointReq]\t" + sourcePositions)
        
                     // TODO: Collection information
//          
//          // Filters the references that are not in the input folders!
//          val posToSave = uOS.positions.filter(srcPos =>
//            c.inputFolders.map( folder => srcPos.file.startsWith(folder)).reduce(_ || _)            
//            )
//          if(posToSave.size > 0){
//            // Raw where used data (contains uses in other files)
//            token.set(whereUsed)(posToSave.map({
//            srcPos => 
//                new org.codeprose.api.TokenProperties.ERangePosition(
//                srcPos.file,
//                srcPos.offset,
//                srcPos.start,
//                srcPos.end
//                )}
//            )
//            )
//            
//            // TODO global where used map typeId -> List[SrcPos]
//            
//            
//          }   
//        } 
        
        token
      }
      case None => { token }
    }
  }
  
  /**
   * Enriches the tokens with information about implicit parameters and conversions.
   * @param file  File for information request.
   * @param tokens Tokens to enrich.
   * @return Tokens augmented with implicit information. 
   */
  private def enrichTokensWithImplicitInformation(
      file: File, 
      tokens: ArrayBuffer[Token]) : ArrayBuffer[Token] = {
    
    val end = tokens.last.offset + tokens.last.length
    val implicitInfo = ensimeClient.implicitInfoReq(file,OffsetRange(0,end))
    
    try {
      val result = Await.result(implicitInfo,  Duration(c.timeout_ImplicitInfoReq, MILLISECONDS))
         
      result.infos.foreach(  implicitInfo => {
      
         implicitInfo match {
         case implConvInfo : org.ensime.api.ImplicitConversionInfo => {
          enrichTokensWithImplictConversionInformation(tokens,implConvInfo)
        }
        case implParaInfo : ImplicitParamInfo => {
          enrichTokenWithImplicitParameterInformation(tokens,implParaInfo)
        }
        case _ => { logger.error("[ImplicitInformation]\t Unknown ImplicitInformation. Ignored!")}
      }
      })  
      
      tokens
      
    }  catch {
    case timeout : TimeoutException => { 
      logger.error("[RequestError] [Timeout] \tImplicitInfoReq:\t" + timeout.getMessage)       
      }
    case e : Throwable => {
      logger.error("[RequestError] \tImplicitInfoReq: \t" + e.getMessage)
      }
 
    }
           
    tokens  
  }
  
  /**
   * Enriches a token with implicit conversion information.
   * @param tokens  Tokens to be enriched.
   * @param info    org.ensime.api.ImplicitConversionInfo
   * @return        Enriched tokens.    
   */
	private def enrichTokensWithImplictConversionInformation(
      tokens: ArrayBuffer[Token], 
      implicitInfo: org.ensime.api.ImplicitConversionInfo) : ArrayBuffer[Token] = {

      val idxAffectedTokens = ProviderUtil.findIndicesOfTokensInRange(
          tokens,implicitInfo.start,implicitInfo.end)
    
      if(idxAffectedTokens.size!=0){
         for(idx <- idxAffectedTokens){
           
              // Save information
              //            println("[--------------------------------")
              //            println(info.fun.`type`.fullName)
              //            println(info.fun.`type`.name)
              //            println(info.fun.`type`.args)
              //            println(info.fun.`type`.typeArgs)
              //            println(info.fun.`type`.declaredAs)
              //            println(info.fun.`type`.declAs)
              //            println(info.fun.`type`.typeId)
              //            println("[--------------------------------")
              // TODO: Upgrade to ImplicitConversion to include more information, like:
              //   - Type id
              //   - Argument names ...
            
            // TODO: HANDLE MULTIPLE IMPLICIT CONVERSIONS for the same token!!
            
              //   - ...
              //            tokens(idx).set(implicitConversion_indicator)(true)
              //            tokens(idx).set(implicitConversion_fullName)(info.fun.name)
              //           
              //            if(info.fun.declPos.isDefined && info.fun.declPos.get.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
              //             tokens(idx).set(implicitConversion_sourcePosition)( new org.codeprose.api.TokenProperties.SourcePosition(
              //                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath,
              //                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].offset)
              //             )
              //             }
         }
      }    
      
      tokens
      
			
	}
  /**
   * Enriches the tokens with implicit parameter information.
   * @param   tokens  Tokens to enriched.
   * @param   implicitInfo  org.ensime.api.ImplicitParamInfo.
   */
	private def enrichTokenWithImplicitParameterInformation(
			tokens: ArrayBuffer[Token], 
			implicitInfo: org.ensime.api.ImplicitParamInfo) : ArrayBuffer[Token] = {
			
     val idxAffectedTokens = ProviderUtil.findIndicesOfTokensInRange(
          tokens,implicitInfo.start,implicitInfo.end)
    
      if(idxAffectedTokens.size!=0){
         for(idx <- idxAffectedTokens){
                 //          println("[--------------------------------------------]")
                //          println(info + "\n")
                //          println("start:\t\t" + info.start)
                //          println("end:\t\t" + info.end)
                //          println("fun:\t\t" + info.fun)
                //          println("params:\t\t " + info.params)
                //          println("funIsImplicit:\t\t" + info.funIsImplicit)
                //          println("[--------------------------------------------]\n")
                          

              // TODO: Allow for multiple implicit parameter per TOKEN!!!
              //              if(tokens(idx)(tokenType).isDefined && tokens(idx)(tokenType).get.isId){
              //                tokens(idx).set(implicitParameter_indicator)(true)
              //                tokens(idx).set(implicitParameter_fullName)("TO BE ADDED")
              //                tokens(idx).set(implicitParameter_sourcePosition)(new org.codeprose.api.TokenProperties.SourcePosition("filename",42))
              //                // TODO: Add more and CORRECT information!   
              //              }
           
         }
      }
     tokens
    
	}
    
  
  /**
   * Enriches the tokens with information about symbol designation.
   * @param file  File for information request.
   * @param tokens  Tokens to enrich.
   * @return    Tokens augmented with symbol designation information. 
   */
  private def enrichTokensWithSymbolDesignations(
      file: File, tokens: ArrayBuffer[Token]) : ArrayBuffer[Token] = {
    
    val symbolDesignationsOption = perfomSymbolDesignationReq(file, 0, tokens.last.offset + tokens.last.length)
    
    symbolDesignationsOption match {
      case Some(symbolDesignations) => {
        import org.codeprose.api.ScalaLang._
        import org.codeprose.api.ScalaLang.SourceSymbol
    
        symbolDesignations.syms.foreach { symDes => {
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
      case None => { tokens }
    }
  }
  
  /**
   * Performs a SymbolInfoReq.
   * @param file  File
   * @param point Offset
   * @return    Some(SymbolInfo)
   */
  private def performSymbolInfoRequest(file: File, point: Int) : Option[org.ensime.api.SymbolInfo] = {
    
    val symbolInfo = ensimeClient.symbolAtPoint(file, point)
   
    val ret = try {
      val result = Await.result(symbolInfo,  Duration(c.timeout_SymbolInfoReq, MILLISECONDS))
      Some(result)
         
      } catch {
        case timeout : TimeoutException => {
          logger.error("[RequestError] [Timeout]\tSymbolInfosReq:\t" + timeout.getMessage)
          None
        }
        case e : Throwable => {
          logger.error("[RequestError] \tSymbolInfosReq:\t" + e.getMessage)
          None
        }
    }
    ret  
  }
  
  /**
   * Performs a UsesOfSymbolAtPointReq.
   * @param file  File
   * @param point Offset
   * @return      Option[ERangePositions] Source positions.
   */
  private def performUsesOfSymbolReq(
      file: File, point: Int) : Option[org.ensime.api.ERangePositions] = {
      
    val usesOfSymbol = ensimeClient.usesOfSymAtPoint(file, point)
    try {
      val result = Await.result(usesOfSymbol,  Duration(c.timeout_UsesOfSymbolAtPointReq, MILLISECONDS))
      Some(result)
    } catch {
      case timeout : TimeoutException => {
        logger.error("[RequestError] [Timeout]\tUsesOfSymbolAtPointReq:\t" + timeout.getMessage)
        None
      }
      case e : Throwable => {
        logger.error("[RequestError] \tUsesOfSymbolAtPointReq:\t" + e.getMessage)
        None
      }
    }
  }
  
  /**
   * Performs a SymbolDesignationReq with all symbol types requested.
   * @param file  File to be checked.
   * @param 
   * @return      Option[org.ensime.api.SymbolDesignations]
   */
  private def perfomSymbolDesignationReq(file: File, start: Int, end: Int) : Option[org.ensime.api.SymbolDesignations] = {
    
    val symbolDesignations = ensimeClient.symbolDesignations(file, start, end, org.ensime.api.SourceSymbol.allSymbols) 
    
    try {
      val result= Await.result(symbolDesignations,  Duration(c.timeout_SymbolDesignationsReq, MILLISECONDS))
      Some(result)
    }  catch {
      case timeout : TimeoutException => { 
        logger.error("[RequestError] [Timeout] \tSymbolDesignationsReq:\t" + timeout.getMessage)
        None
      } 
      case e : Throwable => {
        logger.error("[RequestError] \tSymbolDesignationsReq:\t" + e.getMessage)
        None
      }
    }
  }
    
  
  
  
  /**
   * Collections detailed type information for all types in the project.
   * @return
   * 
   * Notes:
   * Uses occuringTypeIds.
   */
  private def getDetailedTypeInformation() : Map[Int,Option[TypeInformation]] = {
    
    // Some filtering?
    // TODO Remove after debugging
//    println("Raw typeIds found w/ name:")
//    getOccuringTypesWithName().foreach(e => {
//      println(e._1 + "\t" + e._2)
//    })
    
    val detailedTypeInfo = getOccuringTypesWithName().map(e => {
      
      val typeInspectInfo = performInspectTypeByIdReq(e._1)
      
      import org.codeprose.util.EnsimeApiToCodeproseApi
      val typeInformation = org.codeprose.util.EnsimeApiToCodeproseApi.TypeInspectInfoToTypeInformation(typeInspectInfo)
      (e._1, typeInformation)      
    }).toMap
   // TODO Remove after debugging
//    println("\n\nInspectTypeInfo:")
//    detailedTypeInfo.foreach(e => {
//      println(e._1 +"\t" + e._2)
//    })
        
    detailedTypeInfo
  }
  
  /**
   * 
   */
  private def getWhereUsedByTypeIdInformation() : Map[Int,List[ERangePositionWithTokenIds]] = {
    // TODO: ONLY FAKE!!!
     getOccuringTypesWithName().map{ e => 
       (e._1,List[ERangePositionWithTokenIds](
           new ERangePositionWithTokenIds("pathToFile1.scala",0,0,41,List(66,67)),
           new ERangePositionWithTokenIds("pathToFile2.scala",0,0,41,List(1,6)))) }.toMap   
 }
  
  /**
   * Performs a InspectTypeByIdReq.
   * @param typeId  Internal type id assigned to type by ensime-server.
   * @return        Option[org.ensime.api.TypeInspectInfo] 
   * 
   */
  private def performInspectTypeByIdReq(typeId: Int) : Option[org.ensime.api.TypeInspectInfo] = {
		  val typeInspectInfo = ensimeClient.inspectTypeById(typeId) 

      println("Waiting before next InspectTypeReq is send (" + c.pauseBetweenReq_InspectTypeById + " ms)")
      Thread.sleep(c.pauseBetweenReq_InspectTypeById)
      
      
				  try {
					  val result = Await.result(typeInspectInfo,  Duration(c.timeout_InspectTypeByIdReq, MILLISECONDS))
							  Some(result)
				  }  catch {
				    case timeout : TimeoutException => { 
					    logger.error("[RequestError] [Timeout]\tInspectTypeByIdReq:\t" + timeout.getMessage)
					    None
				    }
				    case e : Throwable => {
              logger.error("[RequestError] \tInspectTypeByIdReq:\t" + e.getMessage)
					    None
				    } 
				  }
  }
    
  
  // ==============================================================================
  // Boundary NEW OLD
  // ==============================================================================
  
  
  
  
  
  
//  private def getDetailedTypeInformation() : Unit = {
//    
//    println("Occuring Type Ids: [ALL] " + occuringTypeIds.map(e => e._1 + ": " + e._2).mkString("\n") + "\n")
//    
//    val occurIdFiltered = occuringTypeIds.filter(e => (e._2.contains("rational")))
//    println("Occuring Type Ids: [RATIONAL]" + occurIdFiltered.map(e => e._1 + ": " + e._2).mkString("\n") + "\n")
//
//    
////    occurIdFiltered.map(e=>e._1).map(typeId => {
////      (typeId,performInspectTypeByIdReq(typeId))
////      }).foreach(e=> { 
////        println(e._1 + ": " + e._2 +"\n")
////      }) 
//    
//      // 
//      
//      println("Individual Req: 1 [XXX]\n " + performInspectTypeByIdReq(7) + "\n\n")
//      Thread.sleep(2000)
//      println("Individual Req: 2 " +performInspectTypeByIdReq(7))
//      Thread.sleep(2000)
//      println("Individual Req: 3 " +performInspectTypeByIdReq(7))
//  }
  
//  private def performInspectTypeByIdReq(typeId: Int) : String = {
//    val typeInspectInfo = ensimeClient.inspectTypeById(typeId) 
//   
//    try {
//      val result = Await.result(typeInspectInfo,  Duration(c.timeout_InspectTypeByIdReq, MILLISECONDS))
//      println("InspectTypeById: got result, companionId: " + result.companionId + " - numInterfaces: " + result.interfaces.size)
//      
//      println("===================== - Beg\n")
//      println("compID: " + result.companionId)
//      println("\n")
//      println("typeInfo: " + result.`type`)
//      println("\n")
//      val interStr = result.supers.map( e => "TypeInfo: " + 
//          "\tname: \t" + e.`type`.name + "\n" +
//          "\ttypeid: \t" + e.`type`.typeId + "\n" +
//          "\tdeclAs: \t" + e.`type`.declAs + "\n" +
//          "\tfullName: \t" + e.`type`.fullName + "\n" +
//          "\ttypeArgs: \t" + e.`type`.typeArgs + "\n" +
//          "\tmembers: \t\n" + e.`type`.members.map(e => e.toString() +"\t\t").mkString("\n") + "\n" +
//          "\tpos: \t" + e.`type`.pos + "\n" +
//          "\touterTypeId: \t" + e.`type`.outerTypeId + "\n" +
//          "\n\nViaView: " + e.viaView + "\n----------\n" )
//      println("interfaces:\n" + interStr.mkString("\n"))
//      
//      println("\n===================== - End:\n")
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
//        val siz = iTI.supers.size
//        val typ = iTI.`type`
//        val inter = iTI.interfaces.toList
//       // println(inter)
//       // s = typ.toString()
//        s=iTI.toString() + "\n__" + siz
//      }
//      
//    })
//    
//     typeInspectInfo.onFailure({
//        case t => {(logger.error("SymbolDesignationsReq failed! " + t))}
//      })
//      
//     s
//  }
  
//  private def performInspectTypeAtPoint(file: File, range: OffsetRange) : String = {
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
//    ???
//  }
  
  
//  private def getSymbolDesignations(file: File, tokens: ArrayBuffer[Token]) : ArrayBuffer[Token] = {
//    val end = tokens.last.offset + tokens.last.length
//    val symbolDesignations = ensimeClient.symbolDesignations(file, 0, end, org.ensime.api.SourceSymbol.allSymbols) 
//    
//    try {
//    	val cIResult= Await.result(symbolDesignations,  Duration(c.timeout_SymbolDesignationsReq, MILLISECONDS))
//    }  catch {
//    case timeout : TimeoutException => { 
//    	logger.error("SymbolDesignationsReq:\t" + timeout.getMessage)     	
//      }
// 
//    }
//
//    
//    symbolDesignations.onSuccess({
//      
//      case symDes : SymbolDesignations => {
//        logger.info("Adding SymbolDesignations ... ")
//       // enrichTokensWithSymbolDesignations(tokens,symDes)
//      }
//      
//    })
//    
//     symbolDesignations.onFailure({
//        case t => {(logger.error("SymbolDesignationsReq failed! " + t))}
//      })
//    
//    tokens
//  } 
  
//  private def getImplicitInformation(file: File, tokens: ArrayBuffer[Token]) : ArrayBuffer[Token] = {
//    
//    logger.info("Adding ImplicitInformation ... ")
//    
//    val end = tokens.last.offset + tokens.last.length
//    val implicitInfo = ensimeClient.implicitInfoReq(file,OffsetRange(0,end))
//    
//    try {
//      val resultImplicitInfo = Await.result(implicitInfo,  Duration(c.timeout_ImplicitInfoReq, MILLISECONDS))
//    }  catch {
//    case timeout : TimeoutException => { 
//      logger.error("ImplicitInfoReq:\t" + timeout.getMessage)       
//      }
// 
//    }
//    
//    implicitInfo.onSuccess({
//      
//      case implInfo : ImplicitInfos => {
//         enrichTokensWithImplicitInformation(tokens, implInfo)
//       }
//      case _ => {
//          (logger.error("ImplicitInfoReq failed! "))
//        }
//      
//    })
//    
//     implicitInfo.onFailure({
//        case t => {(logger.error("ImplicitInfoReq failed! " + t))}
//      })
//    
//    tokens
//  }
  
  
  
  /*
   * Enriches the token with additional information obtained from ensime-server.
   */
//	private def enrichToken(file: File, token: org.codeprose.api.Token) : org.codeprose.api.Token = {
//		  
//      import org.codeprose.api.ScalaLang._      
//      
//			val tokenTyp = token(tokenType)
//
//			tokenTyp match {
//			  case Some(tt) => {
//          
//          if(tt.isId){
//            tt match {
//             case Tokens.VARID => { 
//               enrichToken_VARID(file,token)              
//             } 
//             case _ => {
//               enrichToken_Id(file,token)
//             }
//            }
//            
//          } else {
//          
//				  tt match {       
//				    case _ => { 
//            if(c.verbose){ 
//              //logger.info("No information requested for " + tt) 
//            } 
//          }
//         }
//				} 
//			}
//			case _ => { 
//				  logger.error("Oops: Not able to determine the token type of " + token.toPrettyString())
//			  }
//			} 
//
//      token
//      ???
//	}

//  private def enrichToken_VARID(file: File, token: org.codeprose.api.Token) : org.codeprose.api.Token = {
//      import org.codeprose.api.ScalaLang._      
//      import org.codeprose.api.TokenProperties.SourcePosition    
//      //println(file + " - " + OffsetRange(token.offset) + token.text)
//     
//        
//      // SymbolInfoReq
//      val symbolInfo = ensimeClient.symbolAtPoint(file, token.offset)
//      
//      
//      // Awaiting the symbolInfo
//      try {
//        val cIResult= Await.result(symbolInfo,  Duration(c.timeout_SymbolInfoReq, MILLISECONDS))
//      } catch {
//        case timeout : TimeoutException => {
//          logger.error("[RequestError] \tSymbolInfosReq:\t" + timeout.getMessage)       
//        }
//     
//      }
//     // 
//      symbolInfo.onSuccess({
//        case sI: SymbolInfo => {
//          
//         
//          val typeInfo = sI.tpe
//          if(typeInfo.outerTypeId.isDefined)
//            token.set(outerTypeId)(typeInfo.outerTypeId.get)
//          
//          token.set(fullName)(typeInfo.fullName)
//            
//          token.set(typeId)(typeInfo.typeId)
//          /* Inspect Type Stuff - Beg */
//          
////          if(!occuringTypeIds.contains(typeInfo.typeId)){
////            println(typeInfo.typeId + ": " + performInspectTypeAtPoint(file, OffsetRange(token.offset,token.offset)))
////          }
//          /* Inspect Type Stuff - End */
//          
//          occuringTypeIds += ((typeInfo.typeId,typeInfo.fullName))
//          
//          
//          token.set(declaredAs)(typeInfo.declAs.toString)
//          
//          if(typeInfo.args.size>0)                      
//            token.set(args)(typeInfo.args.mkString(","))
//          if(typeInfo.members.size >0) 
//            token.set(members)(typeInfo.members.mkString(","))
//          
//          if(typeInfo.typeArgs.size > 0)
//            token.set(typeArgs)(typeInfo.typeArgs.mkString(","))
//                    
//          // TypeInfo
//          if(typeInfo.isInstanceOf[BasicTypeInfo]){    
//            token.set(isArrowType)(false)
//          } else if (typeInfo.isInstanceOf[ArrowTypeInfo]){        
//            token.set(isArrowType)(false)
//          }
//          
//          if(sI.declPos.isDefined && sI.declPos.get.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
//                    token.set(declaredAt)(
//                        new SourcePosition(sI.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath,
//                        sI.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].offset))
//            
//          }
//        }
//      })
//      
//       symbolInfo.onFailure({
//        case t => {
//          (logger.error("[RequestError] \tSymbolInfoReq failed! " + t))
//          }
//      })
//      
//      // ==================================
//      // Uses of Symbol
//      
//      
//      val usesOfSymbol = ensimeClient.usesOfSymAtPoint(file, token.offset)
//      
//      
//      // Awaiting the symbolInfo
//      try {
//        val cIResult= Await.result(usesOfSymbol,  Duration(c.timeout_SymbolInfoReq, MILLISECONDS))
//      } catch {
//        case timeout : TimeoutException => {
//          logger.error("[RequestError] \tUsesOfSymbolAtPointReq:\t" + timeout.getMessage)       
//        }
//     
//      }
//      
//      usesOfSymbol.onSuccess({
//        case uOS: ERangePositions => {
//          
//          // Filters the references that are not in the input folders!
//          val posToSave = uOS.positions.filter(srcPos =>
//            c.inputFolders.map( folder => srcPos.file.startsWith(folder)).reduce(_ || _)            
//            )
//          if(posToSave.size > 0){
//            // Raw where used data (contains uses in other files)
//            token.set(whereUsed)(posToSave.map({
//            srcPos => 
//                new org.codeprose.api.TokenProperties.ERangePosition(
//                srcPos.file,
//                srcPos.offset,
//                srcPos.start,
//                srcPos.end
//                )}
//            )
//            )
//            
//            
//            
//            
//          }   
//        } 
//      })
//      
//       usesOfSymbol.onFailure({
//        case t => {
//          (logger.error("[RequestError] \tUsesOfSymbolAtPointReq failed! " + t))
//          }
//      })
//      
//      
//      
//      token
//  }
  
//  private def enrichToken_Id(file: File, token: org.codeprose.api.Token) : org.codeprose.api.Token = {
//   ???
//  }
//	
  
//  private def enrichTokensWithImplicitInformation(
//      tokens: ArrayBuffer[Token], 
//      implicitInfos: ImplicitInfos) : ArrayBuffer[Token] = {
//    
//    import org.codeprose.api.ScalaLang._
//    
//    implicitInfos.infos.foreach( { implicitInfo => {
//      
//      implicitInfo match {
//         case info : ImplicitConversionInfo => {
//                     
//          // Find affected tokens
//          var idx_searchStart = 0
//          var idx = 0
//          
//          while(idx != -1 && idx_searchStart<tokens.length){
//                         
//            idx = tokens.indexWhere({ t =>
//             val tPos = t.offset+t.length/2
//             info.start <= tPos && info.end > tPos
//            },idx_searchStart)
//            
//            // Save information
//            if(idx != -1){
//              
//              // Save information
////            println("[--------------------------------")
////            println(info.fun.`type`.fullName)
////            println(info.fun.`type`.name)
////            println(info.fun.`type`.args)
////            println(info.fun.`type`.typeArgs)
////            println(info.fun.`type`.declaredAs)
////            println(info.fun.`type`.declAs)
////            println(info.fun.`type`.typeId)
////            println("[--------------------------------")
//            // TODO: Upgrade to ImplicitConversion to include more information, like:
//            //   - Type id
//            //   - Argument names ...
//            //   - ...
//            tokens(idx).set(implicitConversion_indicator)(true)
//            tokens(idx).set(implicitConversion_fullName)(info.fun.name)
//            if(info.fun.declPos.isDefined && info.fun.declPos.get.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
//             tokens(idx).set(implicitConversion_sourcePosition)( new org.codeprose.api.TokenProperties.SourcePosition(
//                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath,
//                 info.fun.declPos.get.asInstanceOf[org.ensime.api.OffsetSourcePosition].offset)
//             )
//           }
//              
//            }
//            // Search for more tokens
//            idx_searchStart = idx + 1 
//              
//          }
//        }
//        case info : ImplicitParamInfo => {
////        	println("[--------------------------------------------]")
////        	println(info + "\n")
////        	println("start:\t\t" + info.start)
////        	println("end:\t\t" + info.end)
////        	println("fun:\t\t" + info.fun)
////        	println("params:\t\t " + info.params)
////        	println("funIsImplicit:\t\t" + info.funIsImplicit)
////        	println("[--------------------------------------------]\n")
//        
//          // Find affected tokens
//          var idx_searchStart = 0
//          var idx = 0
//          
//          while(idx != -1 && idx_searchStart<tokens.length){
//                         
//            idx = tokens.indexWhere({ t =>
//             val tPos = t.offset+t.length/2
//             info.start <= tPos && info.end > tPos
//            },idx_searchStart)
//            
//            // Save information
//            if(idx != -1){
//              
//              if(tokens(idx)(tokenType).isDefined && tokens(idx)(tokenType).get.isId){
//                tokens(idx).set(implicitParameter_indicator)(true)
//                tokens(idx).set(implicitParameter_fullName)("TO BE ADDED")
//                tokens(idx).set(implicitParameter_sourcePosition)(new org.codeprose.api.TokenProperties.SourcePosition("filename",42))
//                // TODO: Add more and CORRECT information!   
//              }
//            }
//            // Search for more tokens
//            idx_searchStart = idx + 1 
//              
//          }
//          
//        }
//      }
//      
//      } 
//    })  
//    tokens    
//  }
  
//  private def enrichTokensWithSymbolDesignations(
//      tokens: ArrayBuffer[Token], 
//      symDesignations: SymbolDesignations) : ArrayBuffer[Token] = {
//    
//    import org.codeprose.api.ScalaLang._
//    
//    symDesignations.syms.foreach { symDes => {
//      val idx = tokens.indexWhere({ t =>
//         val idx = t.offset+t.length/2
//         symDes.start <= idx && symDes.end > idx
//        },0)
//        if(idx != -1){
//          tokens(idx).set(symbolDesignation)(SourceSymbol.mapEnsimeToCodeprose(symDes.symType))
//        }
//      } 
//    }    
//    tokens    
//  }
 
//  
//  private def includeTokenBasedSourcePostion_declaredAt(info : ArrayBuffer[(File,ArrayBuffer[Token])]) : 
//  ArrayBuffer[(File,ArrayBuffer[Token])] = {
//    import org.codeprose.api.ScalaLang._
//    
//    
//   info.map(e =>{
//     val file = e._1
//     val tokens = e._2
//
//     
//     tokens.map(t => {
//       if(t(declaredAt).isDefined){
//         val srcPos = t(declaredAt).get
//         val tId = findInternalTokenIdToOffset(srcPos,info)
//         if(tId != -1){
//          t.set(declaredAt_TokenIdSrcPos)( new SourcePositionWithTokenId(srcPos.filename,tId))
//         }
//         t
//       } else {
//         t
//       }
//     })
//     
//     (file,tokens)
//   })
//        
//  }

//   private def includeTokenBasedSourcePostion_whereUsedWithinFile(info : ArrayBuffer[(File,ArrayBuffer[Token])]) : 
//  ArrayBuffer[(File,ArrayBuffer[Token])] = {
//    import org.codeprose.api.ScalaLang._
//    
//    
//   info.map(e =>{
//     val file = e._1
//     val tokens = e._2
//
//     
//     tokens.map(t => {
//       if(t(whereUsed).isDefined){
//         
//         val srcPosList = t(whereUsed).get
//         val tIds = srcPosList.filter(srcPos => srcPos.filename == file.getAbsolutePath()).map( srcPos => findInternalTokenIdToOffset(srcPos,info))
//         if(tIds.length >0){
//           t.set(whereUsed_WithinFileTokenIdSrcPos)(tIds.map(id => new SourcePositionWithTokenId(file.getAbsolutePath,id)))
//         }
//         t
//       } else {
//         t
//       }
//     })
//     
//     (file,tokens)
//   })
//        
//  }
  
 
  
}




/**
 * Utils for Provider.
 */
object ProviderUtil {
  
  /**
   * Returns the internal token id held by the token found at the offset position in the file.
   * @param filename
   * @param offset >0
   * @return If token found internal token id, else -1.
   */
  def getTokenIdToOffsetSourcePosition(
      filename: String, offset: Int, info: ArrayBuffer[(File,ArrayBuffer[Token])]) : Int = {
        
    val poentialTokens = info.filter({e => e._1.getAbsolutePath == filename}).map(e=>e._2)
    
    val id = if(poentialTokens.length==0){
      -1
    } else {      
      
      val t = poentialTokens(0).filter(t => {
        t.offset == offset
      })
      
      import org.codeprose.api.ScalaLang._
      
      val ret = if(t.length>0 && t(0)(internalTokenId).isDefined){
        t(0)(internalTokenId).get
      } else {
        -1
      }
      ret
    }
    id 
  } 
  
  /**
   * Find the indices of tokens in the offset range defined by start to end
   * @param tokens  Tokens to be searched
   * @param start   Source offset start
   * @param end     Source offset end
   * @return        List[Int] of tokens with offset in [set,end]
   */
  def findIndicesOfTokensInRange(tokens: ArrayBuffer[Token], start: Int, end: Int) : List[Int] = {
        // Find affected tokens
      var idx_searchStart = 0
      var idx = 0
      var indices = List[Int]()
      
      
      while(idx != -1 && idx_searchStart<tokens.length){

        idx = tokens.indexWhere({ t =>
          val tPos = t.offset+t.length/2
          start <= tPos && end > tPos
          },idx_searchStart)

          // Save information
          if(idx != -1){
            indices = indices :+ idx  
            }
            // Search for more tokens
            idx_searchStart = idx + 1 
          }
      indices
  }
  
}

trait OccuringTypes {
  
   private val occuringTypeIdsWithName = scala.collection.mutable.SortedSet[(Int,String)]() 
//   private val whereUsedCollection = scala.collection.mutable.Map[Int,scala.collection.mutable.SortedSet[ERangePositionWithTokenIds]]()
  
   def addOccuringType(typeId: Int, fullname: String) : Unit = {
     occuringTypeIdsWithName += ((typeId,fullname))
   }
   
   def getOccuringTypesWithName() : scala.collection.mutable.SortedSet[(Int,String)] = {
     occuringTypeIdsWithName
   } 
   
//   def getWhereUsed(typeId: Int) : Option[scala.collection.mutable.SortedSet[ERangePositionWithTokenIds]] = {
//     whereUsedCollection.get(typeId)
//   }
   
}

