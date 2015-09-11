package org.codeprose.provider.scalalang

import org.ensime.client.Client
import java.io.File
import com.typesafe.scalalogging.LazyLogging
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import org.ensime.client.ClientContext
import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token
import org.codeprose.provider.util.EnsimeApiToCodeproseApi
import org.codeprose.api._
import org.codeprose.api.scalalang._
import org.codeprose.provider.util._
import org.codeprose.provider.Provider
import org.codeprose.provider.ProviderContext
import org.codeprose.util.FileUtil

/**
 * Contains information for the EnsimeProvider and 
 * request options for the ensime-client.
 * 
 * - Number of lines in the source code samples.
 * - Timeouts for the async ensime-client requests. 
 * 
 * @param host          Ensime-server address.
 * @param port          Ensime-server port.
 * @param verbose       Print verbose.
 * @param inputFolders  List of folders from which files are processed. 
 *                      Used to determinate if source position is in project. 
 */
class EnsimeProviderContext(
		val host: String,
		val port: Int,
		val verbose: Boolean,
    val inputFolders: List[String]    
		) extends ProviderContext {
  
  // Lines to be selected in source samples
  val whereUsedSourceCodeSamplesNumberOfLinesBefore = 2
  val whereUsedSourceCodeSamplesNumberOfLinesAfter = 2
  
  // All below in ms
  val timeout_ConnectionInfoReq = 500
  val timeout_SymbolInfoReq = 750
  val timeout_SymbolDesignationsReq = 700
  val timeout_ImplicitInfoReq = 700
  val timeout_UsesOfSymbolAtPointReq = 700
  val timeout_InspectTypeByIdReq = 1500
  val timeout_InspectPackageByPathReq = 700
  
  val pauseBetweenReq_InspectTypeById = 250
}


/**
 * The EnsimeProvider uses a org.codeprose.provider.util.ScalaTokenizer to generate
 * tokens and enriches them with information via a connection to an ensime-server.
 * 
 * Usage: 
 *  - Call order: 
 *    1.) initialize()
 *    2.) getProjectInformation() or enrichProjectInformation()
 *    3.) close()
 * 
 * 
 * Information collected:
 *   
 *  - ProjectSummary.enrichedTokens:
 *    
 *    Tokens are generated with org.codeprose.provider.util.ScalaTokenizer.
 *    
 *    If no connection is established with the server the ScalaTokenizer output is returned
 *    without further information.
 *    
 *    Else the following org.codeprose.api.scalang.ScalaLang keys are used:
 *      - tokenType
 *      - typeId
 *      - internalTokenId
 *      - declaredAt
 *      - fullName
 *      - symbolDesignation
 *      - implicitConversionIndicator
 *      - implicitConversionIds
 *      - implicitParameterIndicator
 *      - implicitParameterIds
 *      - whereUsedWithInFile
 *      - isCallable
 *      - ownerTypeId
 *   
 *  - ProjectSummary.summary:
 *  
 *    If no connection is established to the server no summary information is saved.
 *    
 *    Else the following org.codeprose.api.scalang.ScalaLang keys are used:
 *      - fileList
 *      - packageNamePerFile
 *      - packageInformation
 *      - typeInspectInformation
 *      - whereUsedByTypeId
 *      - whereUsedByTypeIdWithCodeSample
 *      - implicitConversionInformation
 *      - implicitParameterInformation
 *  
 * @param c EnsimeProviderContext contains the information needed to setup the EnsimeProvider.
 *          Most importantly, it includes <host:port> of the ensime server.
 *     
 */
class EnsimeProvider(implicit c: EnsimeProviderContext )
    extends Provider with OccuringTypes with LazyLogging {

	private val ensimeClient = new Client()(new ClientContext(c.host,c.port,false)) 
	var isInitialized = false
  // Internal token id used to reference individual tokens in the project. 
  // All tokens in the project are numbered starting with 0.
  var tokenId = 0
 
  /* Temporary containers for implicit conversion and parameter information 
   * gathered by enrichTokensWithImplicitInformation() which saves ids of the implicit 
   * conversion and parameters.
   * 
   * If not null implicitConverionInfo and implicitParamInfo are added to the project summary
   * in getProjectSummary()
   *  
   */
  private var implicitConversionInfo = Map[Int,ImplicitConversionInfoSummary]()
  private var implicitParamInfo = Map[Int,ImplicitParamInfoSummary]()
 
  
	/**
	 * Initializes the ensime client and tests the connection to the server.
   * 
	 * Sets isInitialized to true if test successful. Tokens are only enriched 
   * with information from ensime if test successful.
   *  
	 */
	def initialize(): Unit = {
			logger.info("Initializing Ensime client ... ")    
			ensimeClient.initialize()         
			logger.info("Done.")
			isInitialized = testConnectionToEnsimeServer()       
	}

	/**
	 * Tests the connection to the ensime server by sending a ConnectionInfoReq.
   *   
	 * @return Boolean connection test success indicator. 
	 */
	private def testConnectionToEnsimeServer() : Boolean = {
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

  /**
   * Closes the ensime client.
   *
   */
	def close() : Unit = {
			ensimeClient.close()
	}

  /**
   * Generates project information. 
   * 
   * @param List of files to be processed. Assumes only Scala files are provided!
   * @return ProjectInfo
   */
   def getProjectInformation(files: List[File]) : ProjectInfo =  {
     
     // Enrich tokens
     val enrichedTokenPerFile = getEnrichedTokens(files)
     
     // Project summary information
     val summary = getProjectSummary(files,enrichedTokenPerFile)
          
     new ProjectInfo(enrichedTokenPerFile,summary)
   }
  
   /**
    * Enriches existing project information.
    * Use when connecting several Providers.
    * @param projectInfo  ProjectInfo 
    * @return             ProjectInfo
    */
   def enrichProjectInformation(projectInfo: ProjectInfo) : ProjectInfo =  {
    ???
    }
   
   
   /**
    * Creates project summary information. 
    * 
    * If connection to ensime server is not initialized an empty ProjectSummary is returned.
    * 
    * Collected information:  
    *   - List of files
    *   - Package name per file
    *   - Package information 
    *   - Detailed type information
    *   - Detailed information on where types are used
    *   - 
    * 
    * @param files  files to enrich
    * @param tokens enriched tokens returned from getEnrichedTokens()
    * @return       Project summary
    * 
    */
  private def getProjectSummary(files: List[File], tokens: ArrayBuffer[(File, ArrayBuffer[Token])]) : ProjectSummary = {
    
    val summary = new ProjectSummary()
    if(isInitialized){

      logger.info("Getting project summary information ...")
      
      // Files
      logger.info("\t" + "files")
      summary.set(ScalaLang.fileList)(files)
      
      // Package information
      logger.info("\t" + "package information")
      val packNamesPerFile = getPackageNamesPerFile(tokens)
      val packageNamesUnique = packNamesPerFile.map(e => e._2).toSet.toList
      summary.set(ScalaLang.packageNamePerFile)(packNamesPerFile)
      summary.set(ScalaLang.packageInformation)(getPackageInformaton(packageNamesUnique,tokens))
      
      // Used types
      logger.info("\t" + "type information")
      val tpeInspectInfo = getDetailedTypeInformation(tokens)
      summary.set(ScalaLang.typeInspectInformation)(tpeInspectInfo)
            
      // Where used in project
      logger.info("\t" + "where used information")
      summary.set(ScalaLang.whereUsedByTypeId)(getWhereUsedByTypeIdInformation())
      
      // Where used in project source positions
      logger.info("\t" + "source code samples for where used information")
      summary.set(ScalaLang.whereUsedByTypeIdWithCodeSample)(getSourceSamplesForWhereUsed(tokens,c.whereUsedSourceCodeSamplesNumberOfLinesBefore,c.whereUsedSourceCodeSamplesNumberOfLinesAfter))
      
      // Implicit conversion information
      logger.info("\t" + "implicit conversion information")
      summary.set(ScalaLang.implicitConversionInformation)(implicitConversionInfo)
      
      // Implicit parameter information
      logger.info("\t" + "implicit parameter information")
      summary.set(ScalaLang.implicitParameterInformation)(implicitParamInfo)

      
    }
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
      logger.info("\t" + "adding type information ...")
      val tokens = rawTokens.map( e => {
        val et = e._2.map( t => { enrichToken(t,e._1,rawTokens)}) 
        (e._1,et)
       }) 
        
       
      logger.info("\t" + "adding semantic highlighting information")
      val tokenWithSemantic = tokens.map(e => {
        (e._1,enrichTokensWithSymbolDesignations(e._1,e._2))
      })
      
      logger.info("\t" + "adding implicit information ...")
      val tokenWithSemanticImplicit = enrichTokensWithImplicitInformation(tokenWithSemantic)
      
      tokens
    } else {
      logger.error("Not initialized correctly. Raw Tokens returned.")
      getRawTokens(files: List[File])
    }
    enrichedTokens 
  }
    
    


  /**
   * Generates the raw tokens.
   * @param List of files to process
   * @return Collection of enriched tokens per file. Each Token assigned a unique internalTokenId.
   */
  private def getRawTokens(files: List[File]) : 
  ArrayBuffer[(File,ArrayBuffer[Token])] = {
    
    import org.codeprose.util.FileUtil
    
    logger.info("Generating raw tokens.")
    val tokensPerFile = ArrayBuffer[(File,ArrayBuffer[Token])]()
    
    for(file <- files){
      
      val srcContent =  FileUtil.loadSrcFileContent(file)         
      
      val tokens = ScalaTokenizer.tokenize(srcContent)
      val tokensWithInternalId = tokens.map(t => {
        tokenId+=1
        t.set(ScalaLang.internalTokenId)(tokenId)
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
    
    token(ScalaLang.tokenType) match {
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

  /**
   * 
   * Note assumes token in group IDs.
   */
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
        
        // Collect occurring type information
        val tpeId = symbolInfo.tpe.typeId
        val fullNameStr = symbolInfo.tpe.fullName
        val definitionFilename = symbolInfo.tpe.pos match {
          case Some(pos) => {
            if(pos.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
              Some(pos.asInstanceOf[org.ensime.api.OffsetSourcePosition].file.getAbsolutePath)
            } else {
              None
            }
          }
          case None => { None }
        }
        addOccurringType(tpeId, fullNameStr,definitionFilename)
        
        
              
        // Save type information on token
        // Simple information
        token.set(ScalaLang.typeId)(tpeId)
        token.set(ScalaLang.fullName)(fullNameStr)
        token.set(ScalaLang.isCallable)(symbolInfo.isCallable)
        
        // Owner type id
        symbolInfo.ownerTypeId match {
          case Some(id) => { token.set(ScalaLang.ownerTypeId)(id) }
          case None => { }
        }
                
        
        // Declared At information
        symbolInfo.declPos match {
          case Some(srcPos) => {
            if(srcPos.isInstanceOf[org.ensime.api.OffsetSourcePosition]){
              val offsetSrcPos = srcPos.asInstanceOf[org.ensime.api.OffsetSourcePosition]
              val tokenId = ProviderUtil.getTokenIdToOffsetSourcePosition(offsetSrcPos.file.getAbsolutePath, offsetSrcPos.offset, info)
              token.set(ScalaLang.declaredAt)(OffsetSourcePositionWithTokenId(offsetSrcPos.file.getAbsolutePath,offsetSrcPos.offset,tokenId))
            } else {
              logger.error("[enrichTokenWithSymbolInfo]\t Unknown declaredAt source position type.")
            }
          }
          case None => {}
        }
                
       
       
        
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
        //println("[UsesOfSymbolAtPointReq]\t" + sourcePositions)
        
        // TODO: Generalized to input files???
        // Filter out source positions that are in the dependencies!
        // I.e those not in the input folders
        // Applies e.g. to all Scala symbols (Int,List,...)
        val srcPosWithinProject = sourcePositions.positions.filter(srcPos => {           
            c.inputFolders.map( folder => srcPos.file.startsWith(folder)).reduce(_ || _)            
        })
        
        // Save information
        if(srcPosWithinProject.size>0){
          
          // Translate to codeprose srcPos with token id
          val srcPosWithTokenId = srcPosWithinProject.map( srcPos => {
                // Find referenced token id
                val tokenId = ProviderUtil.getTokenIdToOffsetSourcePosition(srcPos.file, srcPos.offset, info)
                new ERangePositionWithTokenId(srcPos.file,srcPos.offset,srcPos.start,srcPos.end,tokenId)
                })
          
          // Where used within same file
          val srcPosWithinFile = srcPosWithTokenId.filter( srcPos => {
            if(srcPos.filename == file.getAbsolutePath)
              true
            else 
              false
          })
          
          token.set(ScalaLang.whereUsedWithInFile)(srcPosWithinFile)
          
          // Save source positions within type id
          token(ScalaLang.typeId) match {
            case Some(tpeId) => {
              srcPosWithTokenId.foreach( srcPos => {
                addWhereUsedInformation(tpeId, srcPos)  
              } )
              // TODO Save source positions in Map
              // Map[Int,Set[ERangePositionWithTokenId]
              
              //println(tpeId.toString +"\n" + srcPosWithTokenId.map(e=> "\t" + e.toString +"\n"))
              //println("---")
            }
            case None => { }
          }
        }
          
        
        
        // DEBUG
//        if(sourcePositions.positions.size>posToSave.size){
//          println("org size: " + sourcePositions.positions.size + " reduced size: " + posToSave.size)
//          println(file + " -- " + token.text)
//          println(sourcePositions.positions.foreach(e => println(e)))
//          println("-----------------")
//        }
        
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
      tokens: ArrayBuffer[(File,ArrayBuffer[Token])]) : ArrayBuffer[(File,ArrayBuffer[Token])] = {
    
    // Get implicit information for all files
       
    val implicitInfoPerFile = tokens.map(e=>{
          val file = e._1
          val t = e._2
          val end = t.last.offset+t.last.length
          val implicitInfoOpt = performImplicitInfoReq(file, 0, end)
          (file,implicitInfoOpt)
        }).toMap
    
        
    // Save implicit information with id
    /*val implicitConversions = implicitInfoPerFile.map(e=>e._2).
                              flatten.flatMap(e=>e.infos).
                              filter(e=>e.isInstanceOf[org.ensime.api.ImplicitConversionInfo]).
                              map(e=>e.asInstanceOf[org.ensime.api.ImplicitConversionInfo].fun).
                              toSet.zipWithIndex.toMap */ 
    /*                          
    val implicitParams = implicitInfoPerFile.map(e=>e._2).
                              flatten.flatMap(e=>e.infos).
                              filter(e=>e.isInstanceOf[org.ensime.api.ImplicitParamInfo]).
                              map(e=>e.asInstanceOf[org.ensime.api.ImplicitParamInfo].fun).
                              toSet.zipWithIndex.toMap */
    
    // Translate to codeprose api and save
    val apiConverter = new EnsimeApiToCodeproseApi(tokens,ProviderUtil.getTokenIdToOffsetSourcePosition)
       
        
    val implicitConversions = implicitInfoPerFile.map(e=>e._2).
                              flatten.flatMap(e=>e.infos).
                              filter(e=>e.isInstanceOf[org.ensime.api.ImplicitConversionInfo]).
                              map(e=>apiConverter.convertToImplicitConversionInfoSummary(e.asInstanceOf[org.ensime.api.ImplicitConversionInfo])).
                              toSet.zipWithIndex.toMap
    implicitConversionInfo = implicitConversions.map(_.swap)           
   
    val implicitParams = implicitInfoPerFile.map(e=>e._2).
                              flatten.flatMap(e=>e.infos).
                              filter(e=>e.isInstanceOf[org.ensime.api.ImplicitParamInfo]).
                              map(e=>apiConverter.convertToImplicitParamInfoSummary(e.asInstanceOf[org.ensime.api.ImplicitParamInfo])).
                              toSet.zipWithIndex.toMap

    implicitParamInfo = implicitParams.map(_.swap)                              
                             
   /* implicitConversions.foreach(e=>{
      val symInfo =  apiConverter.convertToSymbolInfo(e._1)
      val id = e._2
      implicitConversionInfo += (id -> symInfo)
    })     
    
    implicitParams.foreach(e=>{
      val symInfo =  apiConverter.convertToSymbolInfo(e._1)
      val id = e._2
      implicitParamInfo += (id -> symInfo)
    }) */     
                              
    // Enrich tokens
    tokens.map(e=>{
      val file = e._1
      val t = e._2
      val implicitInfoOpt=implicitInfoPerFile.filter(e=>e._1==file).map(e=>e._2).toList
      
      if(implicitInfoOpt.size>0){
        implicitInfoOpt(0) match {
        case Some(implicitInfos) => {

         implicitInfos.infos.foreach(  implicitInfo => {
      
           implicitInfo match {
            case implConvInfo : org.ensime.api.ImplicitConversionInfo => {
              val implConverSummary = apiConverter.convertToImplicitConversionInfoSummary(implConvInfo)
              val implicitConversionId = implicitConversions.get(implConverSummary).getOrElse(-1)
              enrichTokensWithImplictConversionInformation(t,implConvInfo,implicitConversionId)
            }
            case implParaInfo : org.ensime.api.ImplicitParamInfo => {
              val implParamSummary = apiConverter.convertToImplicitParamInfoSummary(implParaInfo)
              val implicitParamId = implicitParams.get(implParamSummary).getOrElse(-1)
              enrichTokenWithImplicitParameterInformation(t,implParaInfo,implicitParamId)
            }
            case _ => { logger.error("[ImplicitInformation]\t Unknown ImplicitInformation. Ignored!")}
          }
        })  
        t  
        }
        case None => { t }
        }
      } else {t}
      (file,t)
    })
                                    
        
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
      implicitInfo: org.ensime.api.ImplicitConversionInfo,
      implicitConversionId: Int) : ArrayBuffer[Token] = {

      val idxAffectedTokens = ProviderUtil.findIndicesOfTokensInRange(
          tokens,implicitInfo.start,implicitInfo.end)
    
      if(idxAffectedTokens.size!=0){
         for(idx <- idxAffectedTokens){
           
             // Mark token that implicit information is applied
             tokens(idx).set(ScalaLang.implicitConversionIndicator)(true)
           
             // Save ids of implicit conversions
             if(implicitConversionId != -1){
             val ids = tokens(idx)(ScalaLang.implicitConversionIds) match {
               case Some(ids) => {
                   ids :+ implicitConversionId
               } 
               case None => {
                 List(implicitConversionId)               
               }
             }
             tokens(idx).set(ScalaLang.implicitConversionIds)(ids)
           }
                      
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
			implicitInfo: org.ensime.api.ImplicitParamInfo,
      implicitParamId: Int) : ArrayBuffer[Token] = {
			
     val idxAffectedTokens = ProviderUtil.findIndicesOfTokensInRange(
          tokens,implicitInfo.start,implicitInfo.end)
    
      if(idxAffectedTokens.size!=0){
         for(idx <- idxAffectedTokens){
           
           // Mark token that implicit information is applied
           tokens(idx).set(ScalaLang.implicitParameterIndicator)(true)
           
           // Save ids of implicit parameters
           if(implicitParamId != -1){
             val ids = tokens(idx)(ScalaLang.implicitParameterIds) match {
               case Some(ids) => {
                   ids :+ implicitParamId
               } 
               case None => {
                 List(implicitParamId)               
               }
             }
             tokens(idx).set(ScalaLang.implicitParameterIds)(ids)
           }
         }
      }
     tokens
    
	}
    
  
  /**
   * Enriches the tokens with information about symbol designation.
   * @param file    File for information request.
   * @param tokens  Tokens to enrich.
   * @return        Tokens augmented with symbol designation information. 
   */
  private def enrichTokensWithSymbolDesignations(
      file: File, tokens: ArrayBuffer[Token]) : ArrayBuffer[Token] = {
    
    val symbolDesignationsOption = perfomSymbolDesignationReq(file, 0, tokens.last.offset + tokens.last.length)
    
    symbolDesignationsOption match {
      case Some(symbolDesignations) => {
    
        symbolDesignations.syms.foreach { symDes => {
        val idx = tokens.indexWhere({ t =>
           val idx = t.offset+t.length/2
           symDes.start <= idx && symDes.end > idx
          },0)
          if(idx != -1){
            tokens(idx).set(ScalaLang.symbolDesignation)(ScalaLang.SourceSymbol.mapEnsimeToCodeprose(symDes.symType))
          }
        } 
      }    
    tokens  
      } 
      case None => { tokens }
    }
  }
  
  /**
   * Sends a SymbolAtPointReq to the ensime server. 
   * 
   * If request fails or times out None is returned.
   * 
   * @param file  File
   * @param point Offset
   * @return    Some(SymbolInfo)
   */
  private def performSymbolInfoRequest(file: File, point: Int) : Option[org.ensime.api.SymbolInfo] = {
   
    
    val symbolInfo = ensimeClient.symbolAtPoint(file, point)
    
    val requestDetails = s""""Request details: File: """ + file.getAbsolutePath.takeRight(100) + s""" - Point:  $point"""
    
    val ret = try {
      val result = Await.result(symbolInfo,  Duration(c.timeout_SymbolInfoReq, MILLISECONDS))
      Some(result)
         
      } catch {
        case timeout : TimeoutException => {
          logger.error("[RequestError] [Timeout]\tSymbolInfosReq:\t" + timeout.getMessage + "\n" + requestDetails )
          None
        }
        case e : Throwable => {
          logger.error("[RequestError] \tSymbolInfosReq:\t" + e.getMessage + "\n" + requestDetails )
          None
        }
    }
    ret  
  }
  
  /**
   * Sends a UsesOfSymbolAtPointReq to the ensime server. 
   * 
   * If the request fails or times out None is returned.
   * 
   * The UsesOfSymbolAtPointReq returns source code positions where the requested 
   * symbol is also used. 
   * 
   * @param file  File
   * @param point Offset
   * @return      Option[ERangePositions] Source positions.
   */
  private def performUsesOfSymbolReq(
      file: File, point: Int) : Option[org.ensime.api.ERangePositions] = {
    
    val usesOfSymbol = ensimeClient.usesOfSymAtPoint(file, point)
    
    val requestDetails = s""""Request details: File: """ + file.getAbsolutePath.takeRight(100) + s""" - Point:  $point"""
  
    
    try {
      val result = Await.result(usesOfSymbol,  Duration(c.timeout_UsesOfSymbolAtPointReq, MILLISECONDS))
      Some(result)
    } catch {
      case timeout : TimeoutException => {
        logger.error("[RequestError] [Timeout]\tUsesOfSymbolAtPointReq:\t" + timeout.getMessage + "\n" + requestDetails)
        None
      }
      case e : Throwable => {
        logger.error("[RequestError] \tUsesOfSymbolAtPointReq:\t" + e.getMessage + "\n" + requestDetails)
        None
      }
    }
  }
  
  /**
   * Sends a SymbolDesignationReq to the ensime server with all symbol types requested.
   * 
   * If request fails or times out None is returned.
   *  
   * @param file  File to be checked.
   * @param 
   * @return      Option[org.ensime.api.SymbolDesignations]
   */
  private def perfomSymbolDesignationReq(file: File, start: Int, end: Int) : Option[org.ensime.api.SymbolDesignations] = {
    
    val symbolDesignations = ensimeClient.symbolDesignations(file, start, end, org.ensime.api.SourceSymbol.allSymbols) 
    
    val requestDetails = s""""Request details: File: """ + file.getAbsolutePath.takeRight(100) + s""" - Range:  $start - $end"""
    
    try {
      val result= Await.result(symbolDesignations,  Duration(c.timeout_SymbolDesignationsReq, MILLISECONDS))
      Some(result)
    }  catch {
      case timeout : TimeoutException => { 
        logger.error("[RequestError] [Timeout] \tSymbolDesignationsReq:\t" + timeout.getMessage + "\n" + requestDetails)
        None
      } 
      case e : Throwable => {
        logger.error("[RequestError] \tSymbolDesignationsReq:\t" + e.getMessage + "\n" + requestDetails)
        None
      }
    }
  }
    
  /**
   * Sends a ImplicitInfoReq to the ensime server.
   *    
   * If request fails or times out None is returned.
   * 
   * @param file  File
   * @param start Range begin
   * @param end   Range end
   * @return    Some(SymbolInfo)
   */
  private def performImplicitInfoReq(file: File, start: Int, end: Int) : Option[org.ensime.api.ImplicitInfos] = {
     
    val implicitInfo = ensimeClient.implicitInfoReq(file,org.ensime.api.OffsetRange(start,end))
    
    val requestDetails = s""""Request details: File: """ + file.getAbsolutePath.takeRight(100) + s""" - Range:  $start - $end"""
    
    val ret = try {
      val result = Await.result(implicitInfo,  Duration(c.timeout_ImplicitInfoReq, MILLISECONDS))
      Some(result)
         
      } catch {
        case timeout : TimeoutException => {
          logger.error("[RequestError] [Timeout]\tImplicitInfoReq:\t" + timeout.getMessage + "\n" + requestDetails)
          None
        }
        case e : Throwable => {
          logger.error("[RequestError] \tImplicitInfoReq:\t" + e.getMessage + "\n" + requestDetails)
          None
        }
    }
    ret  
  }
  
 
   /**
   * Sends an InspectPackageByPathReq to the ensime server.
   * 
   * If request fails or times out None is returned.
   * 
   * @param   packagePath Package path like org.codeprose.xyz
   * @return              Some(PackageInfo)
   */
  private def performInspectPackageByPathReq(packagePath: String) : Option[org.ensime.api.PackageInfo] = {
     
    val packageInfo = ensimeClient.inspectPackageByPath(packagePath)
    
    val requestDetails = s""""Request details: PackagePath: $packagePath"""
    
    val ret = try {
      val result = Await.result(packageInfo,  Duration(c.timeout_InspectPackageByPathReq, MILLISECONDS))
      Some(result)
         
      } catch {
        case timeout : TimeoutException => {
          logger.error("[RequestError] [Timeout]\tInspectPackageByPathReq:\t" + timeout.getMessage + "\n" + requestDetails)
          None
        }
        case e : Throwable => {
          logger.error("[RequestError] \tInspectPackageByPathReq:\t" + e.getMessage + "\n" + requestDetails )
          None
        }
    }
    ret  
  }
  
  /**
   * Sends an InspectTypeByIdReq to the ensime server.
   * 
   * If request fails or times out None is returned.
   * 
   * @param typeId  Internal type id assigned to type by ensime-server.
   * @return        Option[org.ensime.api.TypeInspectInfo] 
   * 
   */
  private def performInspectTypeByIdReq(typeId: Int) : Option[org.ensime.api.TypeInspectInfo] = {
      
    val typeInspectInfo = ensimeClient.inspectTypeById(typeId) 

    val requestDetails = s""""Request details: TypeId: $typeId"""
    
      //println("Waiting before next InspectTypeReq is send (" + c.pauseBetweenReq_InspectTypeById + " ms)")
      //Thread.sleep(c.pauseBetweenReq_InspectTypeById)
      
      
          try {
            val result = Await.result(typeInspectInfo,  Duration(c.timeout_InspectTypeByIdReq, MILLISECONDS))
                Some(result)
          }  catch {
            case timeout : TimeoutException => { 
              logger.error("[RequestError] [Timeout]\tInspectTypeByIdReq:\t" + timeout.getMessage + "\n" + requestDetails)
              None
            }
            case e : Throwable => {
              logger.error("[RequestError] \tInspectTypeByIdReq:\t" + e.getMessage + "\n" + requestDetails)
              None
            } 
          }
  }
   
  
  
  
  /**
   * Collects detailed type information for all types in the project.
   * @return  Map from type id to Option[TypeInspectInfo].
   * 
   * Notes:
   * Uses occurringTypeIds.
   */
  private def getDetailedTypeInformation(enrichedTokens: ArrayBuffer[(File,ArrayBuffer[Token])]) : 
  Map[Int,Option[TypeInspectInfo]] = {
    
    // The code in this section is included to filter out types
    // with definitions within /.ensime_cache/ to avoid crashing the ensime-client with
    // unparseable responses by the ensime-server.
    //
    //  To remove the filtering just comment the section below and uncomment 
    //     //val typesToInspect = getOccurringTypesWithName() below
    // 
    // ---------------------------------------------
// TODO: Remove after debugging
//    logger.info("Raw types found w/ name filename of definition:")
//    getOccurringTypesWithName().foreach(e => {
//      logger.info("\t" + e._1 + "\t" + e._2 + "\t" + e._3)
//    })
    
    logger.info("[IMPORTANT]\t" + "No type inspect information for types defined in .ensime_cache")
    
    // Filter types to avoid ensime-client crash    
    val typesToInspect = getOccurringTypesWithName().filter(e => {
      val defFilename = e._3.getOrElse("")    
      if(defFilename.contains(".ensime_cache"))
        false
      else 
        true
    })

//    logger.info("Filtered types found w/ name filename of definition:")
//    typesToInspect.foreach(e => {
//      logger.info("\t" + e._1 + "\t" + e._2 + "\t" + e._3)
//    })
        
    // ---------------------------------------------
   
    // If no filtering is applied.
    //val typesToInspect = getOccurringTypesWithName()
   
    val ensimeTypeInspectInfoPerTypeId = typesToInspect.map(e => {
      
      val typeInspectInfo = performInspectTypeByIdReq(e._1)
      (e._1,typeInspectInfo)
    }).toMap
      
    val apiConverter = new EnsimeApiToCodeproseApi(enrichedTokens,ProviderUtil.getTokenIdToOffsetSourcePosition)
    
    val typeInspectInfo = ensimeTypeInspectInfoPerTypeId.map(e=> {
      
     val tpeInspectInfo = e._2 match {
        case Some(tpeInsp) => {
          try{
            Some(apiConverter.convertToTypeInspectInfo(tpeInsp))
          } catch {
            case _ : Throwable => {
              logger.error("Unable to convert TypeInspectInfo.")
              None }
          }
        }
        case None => {
          None
        }
      }
      (e._1,tpeInspectInfo)
      
    })
    
       
    typeInspectInfo
  }
  
  
  /**
   * Returns the where used information per type id.
   * @return  Map from type id to List of source positions.
   */
  private def getWhereUsedByTypeIdInformation() : Map[Int,List[ERangePositionWithTokenId]] = {
       
    getWhereUsedAllTypes().map(e => {
      val sortedSrcPos = e._2.toArray
      // Sorting.quickSort(sortedSrcPos)(Ordering[ERangePositionWithTokenId])
      
      (e._1,sortedSrcPos.toList)
    }).toMap
 }
  
  
  /**
   * Returns package information per packagename.
   * @return Map from package name to package information. 
   */
  private def getPackageInformaton(
      packageNames: List[String],tokens: ArrayBuffer[(File,ArrayBuffer[Token])]) 
  : Map[String,Option[PackageInfo]] = {
    
    val ensimePackageInfo = packageNames.map(name => {
      val packageInfoOpt = performInspectPackageByPathReq(name)
      (name,packageInfoOpt)}).toMap
    
     val apiConverter = new EnsimeApiToCodeproseApi(tokens,ProviderUtil.getTokenIdToOffsetSourcePosition) 
      
    val packageInformation = ensimePackageInfo.map( e=> {
      val packInfo = e._2 match {
        case Some(ensimePackInfo) => {
          try {
            Some(apiConverter.convertToPackageInfo(ensimePackInfo))
          } catch {
            case _ : Throwable => {
              logger.error("Unable to convert PackageInfo.")
              None }
          }
        } 
        case None => { None }
      } 
      (e._1,packInfo)
    }) 
   
    packageInformation
  }
  
  
  /**
   * Extracts the package information from the tokens of each file.
   * @param enrichedTokens  Tokens per file.
   * @return                Map file to package name. 
   */
  private def getPackageNamesPerFile(
      enrichedTokens: ArrayBuffer[(File,ArrayBuffer[Token])]) : Map[File,String] = {
   
    enrichedTokens.map(e => {
      val file = e._1
      val tokens = e._2
        
      // Find package token
      val beg = tokens.indexWhere { t => t(ScalaLang.tokenType).get == ScalaLang.Tokens.PACKAGE }
      var notFound=false
      
      val packageStr = if(beg != -1){
        
        // Find WS token with newline
        val end = tokens.indexWhere({ t => 
          t(ScalaLang.tokenType).get == ScalaLang.Tokens.WS && t.text.contains("\n")},beg)
          
        if(end != 1){
          tokens.slice(beg+1, end).map(e=> e.text).mkString.trim()
        } else { "" }
      } else { "" }
      
    (file,packageStr)
    }).toMap
    
  }
  
  /**
   * Retrieves source code samples for all source positions in where used,
   * @param enrichedTokens  Enriched token information
   * @param numberOfLines   Number of source code lines to retrieve.
   * @return                
   */
    private def getSourceSamplesForWhereUsed(
        enrichedTokens: ArrayBuffer[(File,ArrayBuffer[Token])],
        numberOfLinesBefore: Int,
        numberOfLinesAfter: Int) : 
    Map[Int,List[(ERangePositionWithTokenId, List[String])]] = {
      
      val whereUsedByTypeIdSorted = getWhereUsedAllTypes().map(e=>{
        (e._1,e._2.toList.sorted)
      })
      
      val srcPosPerTypeId = whereUsedByTypeIdSorted.map(e => {
        val sourceSamples = e._2.map( srcPos => {
          // Convert set to 
          (srcPos,getSourceCodeSampleForToken(srcPos,enrichedTokens,numberOfLinesBefore,numberOfLinesAfter))
        }).toList
        (e._1,sourceSamples)
      }).toMap

      srcPosPerTypeId
    }
    
    /**
     * Returns a source code sample matching a token based source position.
     * @param srcPos          Source position.
     * @param enrichedTokens  Enriched tokens.
     * @param numberOfLine    Number of source code lines to retrieve.
     * @return                List of text text before token, token text, text after token
     */
    private def getSourceCodeSampleForToken(
        srcPos: ERangePositionWithTokenId, 
        enrichedTokens: ArrayBuffer[(File,ArrayBuffer[Token])],
        numberOfLinesBefore: Int,
        numberOfLinesAfter: Int) : List[String] = {
      
      // Find tokens 
      val tokens = enrichedTokens.filter(e => 
        if(e._1.getAbsolutePath == srcPos.filename){
          true
        } else{  
          false
        }
        ).map(e=>e._2)
      
       val srcSample = if(tokens.size>0){
          
          // Find idx of token
           val idxMain = ProviderUtil.findIndicesOfTokensInRange(tokens(0),srcPos.start,srcPos.end)
      
//           // DEBUG
//           if(idxMain.size>1){
//             println("[Several tokens found: " + idxMain)
//             println(srcPos) 
//           }
           
           val sampleText = if(idxMain.size>0){
      
          // Find preceding tokens
          val idxNewLines = tokens(0).zipWithIndex.filter { e => 
            if(e._1.text.contains("\n")) 
              true
            else 
              false
            }.map(e=>e._2)
          
          val idxNewLineBefore = idxNewLines.filter(e=> 
              if(e<idxMain(0))
                true
              else 
                false)
          val idxStart = if(idxNewLineBefore.length>numberOfLinesBefore){
            idxNewLineBefore(idxNewLineBefore.length-numberOfLinesBefore)
          } else {
            0
          }

          // Find following tokens
          val idxNewLineAfter = idxNewLines.filter(e=> 
              if(e>idxMain(0))
                true
              else 
                false)

          val idxEnd = if(idxNewLineAfter.length>numberOfLinesAfter){
            idxNewLineAfter(numberOfLinesAfter)
          } else{ tokens(0).length-1 } 
            
          val textBefore = tokens(0).slice(idxStart, idxMain(0)).map(e => e.text).mkString("")
          val textAfter = tokens(0).slice(math.min(idxMain(0)+1,tokens(0).length-1), idxEnd).map(e => e.text).mkString("")
          val textToken = tokens(0)(idxMain(0)).text
          List(textBefore,textToken,textAfter)
        } else {
          logger.error("[getSourceCodeSampleForToken]+\t" + "Token not found.")
          List("")
        }
        sampleText
      } else {
        logger.error("[getSourceCodeSampleForToken]+\t" + "Source file not found.")
        List("")
      }
      srcSample
    }
    
    
    // Test
    private def reInitializeEnsimeClient() : Unit = {
      logger.error("Ensime client seems to be non responsive.")
      logger.error("Reinitializing the ensime client.")
      
      initialize()
      
    }
    
  
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
      
      val ret = if(t.length>0 && t(0)(ScalaLang.internalTokenId).isDefined){
        t(0)(ScalaLang.internalTokenId).get
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

/**
 * Collects information on occurring types and where used information.
 */
trait OccuringTypes {
  
   /**
    * Used to save occurring type id, type full name and filename with definition.
    */
   private val occurringTypeIdsWithName = scala.collection.mutable.SortedSet[(Int,String,Option[String])]()
   /**
    * Used to save where used information by type id.
    */
   private val whereUsedCollection = scala.collection.mutable.Map[Int,scala.collection.mutable.Set[ERangePositionWithTokenId]]()
  
   /**
    * Add type id, type full name and Option of filename of type position from ensime.
    * @param  typeId              Type id.
    * @param  fullname            Full type name.
    * @param  definitionFilename  Filename of definition position returned from ensime.
    * 
    */
   def addOccurringType(typeId: Int, fullname: String, definitionFilename: Option[String]) : Unit = {
     occurringTypeIdsWithName += ((typeId,fullname,definitionFilename))
   }
   
   /**
    * Returns a set containing tuples with type id, type full name, and Option of filename of definition from ensime.
    * @return   Set containing tuples with type id, type full name, and Option of filename of definition from ensime.
    */
   def getOccurringTypesWithName() : scala.collection.mutable.SortedSet[(Int,String,Option[String])] = {
     occurringTypeIdsWithName
   } 
   
   
   /**
    * Add where used information for a type id.
    * @param  typeId  Type id.
    * @param  srcPos  Source position.
    */
   def addWhereUsedInformation(typeId: Int, srcPos: ERangePositionWithTokenId) : Unit = {
     val s = whereUsedCollection.get(typeId)
     s match {
       case Some(srcPosSet) => {
         srcPosSet += srcPos
       }
       case None => {
         whereUsedCollection += (typeId -> scala.collection.mutable.Set[ERangePositionWithTokenId](srcPos))  
       }
     }
   }

  /**
   * Optionally returns a set containing source positions for a type id.
   * @param   typeId  Type id.
   * @return  Option of set with source positions.
   */
  def getWhereUsedByTypeId(typeId: Int) : Option[scala.collection.mutable.Set[ERangePositionWithTokenId]] = {
    whereUsedCollection.get(typeId)
  }
  
  /**
   * Returns map containing source positions for type ids.
   * @retun Map of type ids to set of source positions.
   */
  def getWhereUsedAllTypes() : scala.collection.mutable.Map[Int,scala.collection.mutable.Set[ERangePositionWithTokenId]] = {
    whereUsedCollection
  }
   
}

