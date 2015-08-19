package org.codeprose.consumer

import java.io.File
import org.codeprose.api.ScalaLang._
import org.codeprose.api.Token
import org.codeprose.consumer.util.CommentUtil
import org.codeprose.consumer.util.MarkdownConverter
import org.codeprose.util.FileUtil
import org.codeprose.util.StringUtil
import com.typesafe.scalalogging.LazyLogging
import org.codeprose.consumer.util.OutputContextSetter
import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.ProjectInfo
import org.codeprose.api.ProjectSummary
import org.codeprose.api.TypeInformation
import org.codeprose.util.CodeproseJsonFormat._
import spray.json._
import org.codeprose.api.ERangePositionWithTokenIds


class ResourceRelPaths(val base: String, val target: String)


class WriterContextHtml(
    val verbose: Boolean
    ) extends ConsumerContext(verbose) {  
  val outputFolders = List("content","js","style")
  val styleSheetRelPath = new ResourceRelPaths("/html/style.css","style/style.css")
  val jsGlobalRelPath = new ResourceRelPaths("/js/codeprose.global.js","js/codeprose.global.js")
  val filesToCopy = List[ResourceRelPaths](styleSheetRelPath)
  val summaryFilesRelPath = Map( "index" -> "index.html",
                                 "js.global.typeinfo" -> "/js/codeprose.typeinformation.js",
                                 "js.global.whereusedinfo" -> "/js/codeprose.whereusedinformation.js")
}



class WriterHtml(outputPath: File)(implicit c: WriterContextHtml)
extends Consumer with LazyLogging {
 
  
	def generateOutput(projectInfo: ProjectInfo) : Unit = {
    
      setupOutputContext(outputPath)
    
      generateSummaryPages(projectInfo)
      generateIndividualPages(projectInfo)
    
      // --------------------------------------------------------------------
      // OLD - Stuff - Begin
    
//      val info = projectInfo.enrichedTokens
//      
//      import org.codeprose.util.StringUtil
//			val filenamesShorted = StringUtil.getUniqueShortFileNames(info.map(e => e._1.getAbsolutePath).toList)
//			val outputFilenames = filenamesShorted.map(s => outputPath + "/content/" + s.replace("/","_") + ".html")
//      
//      val filenamesOriginalToOutput =  info.map(e => e._1.getAbsolutePath).zip(outputFilenames).toArray
//      
//      logger.info("Generating output ...")					
//
//      // Output context
//      setupOutputContext(outputPath)
//      
//      generateIndexFile(info.map(e => e._1.getAbsolutePath()).toList,filenamesShorted,outputFilenames)
//      
//			var idx=0
//			for(i<-info){     
//			  generateOutputFile(new File(outputFilenames(idx)),i._1,i._2,filenamesOriginalToOutput)
//				idx+=1
//      }
//			logger.info("Done.")
	}

  
  /**
   * Generates the output for the individual source files.
   * @param projectInfo ProjectInfo 
   */
  private def generateIndividualPages(projectInfo: ProjectInfo) : Unit = {
    logger.info("Generating output for the source files ...")  
  }
  
  /**
   * Generates and writes to disk the projects summary pages (incl. javascript 'databases') 
   * @param projectInfo ProjectInfor
   * 
   */
  private def generateSummaryPages(projectInfo: ProjectInfo) : Unit = {
   
    //generateIndexPage(projectInfo.)
    //generateWhereUsedPage()
    //generateTypeInformationPage()
    
    generateGlobalJSInformationFiles(projectInfo.summary)
    
    
  }
  
  
  private def generateIndexPage(projectSummary: ProjectSummary) : Unit = { 
    
  }
  
  private def generateWhereUsedPage() : Unit = { 
    
  }
  
  private def generateTypeInformationPage() : Unit = { 
    
  }
  
  private def generateGlobalJSInformationFiles(projectSummary: ProjectSummary) : Unit = {
    
    logger.info("Generating global js information file ...")
    
    import org.codeprose.api.ScalaLang.typeInformation
    
    projectSummary(typeInformation) match {
      case Some(tpeInfos) => {
        generateGlobalJSTypeInfo(tpeInfos)
      } 
      case None => { logger.error("No type information provided in project summary.") }
    }
    
    import org.codeprose.api.ScalaLang.whereUsedByTypeId
    
    projectSummary(whereUsedByTypeId) match {
      case Some(whereUsed) => {
        generateGlobalJSWhereUsedInfo(whereUsed)
      } 
      case None => { logger.error("No where used information provided in project summary.") }
    }
    
  }
  
  /**
   * Saves type information to disk in js file.
   * @param typeInfos Type information by type id.
   */
  private def generateGlobalJSTypeInfo(typeInfos: Map[Int,Option[TypeInformation]]) : Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.typeinfo")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(outputPath.getAbsolutePath + relFileName.get)
      logger.info("Type information: \t" + relFileName + " ...")      

      val beg = s"""
        // codeprose
        //
        // type information
        function typeInformation(typeId){ 
          tInfo = null;
          switch(typeId){
          
          """
        
      val end = s"""
        default: \n\t\t tInfo=null;
        }
        return tInfo;
      };"""
      
      val entries = typeInfos.map(e => {
        val typeId = e._1
        e._2 match {
          case Some(tI) => {            
            val jsonStr = tI.toJson.compactPrint
            s"""\t case $typeId:\n\t\ttInfo=""" + jsonStr + s"""; break;"""
          } 
          case None => {""}
        }
        
      }).mkString("\n")
      
      FileUtil.writeToFile(outputFilename,beg+entries+end)      

    } else {
      logger.error("Unable to generate js file with type information. No file name provided!")
    }
    
  }
  
  /**
   * Saves where used information to disk in js file.
   * @param whereUsed Source positions by type id.
   */
  private def generateGlobalJSWhereUsedInfo(whereUsed: Map[Int,List[ERangePositionWithTokenIds]]) : Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.whereusedinfo")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(outputPath.getAbsolutePath + relFileName.get)
      logger.info("Where used information: \t" + relFileName + " ...")      

      val beg = s"""
        // codeprose
        //
        // where used information
        function whereUsedInformation(typeId){ 
          whereUsed = null;
          switch(typeId){
          
          """
        
      val end = s"""
        default: \n\t\t whereUsed=null;
        }
        return whereUsed;
      };"""
      
      val entries = whereUsed.map(e => {
        val typeId = e._1
        val jsonStr = e._2.toJson.compactPrint
        s"""\t case $typeId:\n\t\twhereUsed=""" + jsonStr + s"""; break;"""
        
      }).mkString("\n")
      
      FileUtil.writeToFile(outputFilename,beg+entries+end)      

    } else {
      logger.error("Unable to generate js file with where used information! No file name provided.")
    }
  }
  
  /**
   * Sets up the output context
   * @param outputPath  Main output path.
   */
  private def setupOutputContext(outputPath: File) : Unit = {
    val setter = new OutputContextSetter(outputPath)
    setter.setFolderStructure(c.outputFolders)
    c.filesToCopy.foreach({ f => setter.copyResource(f.base, new File(outputPath,f.target)) }) 
  }
  
  // OLD - BEGIN
  // --------------------------------------------------------------------------------------
  
  
	private def generateIndexFile(
      originalFilenames: List[String], 
      filenamesShortened : List[String], 
      links: List[String]
      ) : Unit = {   
			val outputFilename= new File(outputPath.getAbsolutePath + "/index.html")
      logger.info("Index page: \t" + outputFilename + " ...")      
			val htmlFrame = new HtmlSummaryFileContext()			
			FileUtil.writeToFile(outputFilename,htmlFrame.begin + htmlFrame.getFileListing(originalFilenames,filenamesShortened,links) + htmlFrame.end)      
	} 

	private def generateOutputFile(
			outputFile: File, 
			srcFile: File, 
			info: scala.collection.mutable.ArrayBuffer[Token],
      filenamesOriginalToOutputNames : Array[(String,String)]
      ) : Unit = {

      logger.info("Individual pages ... ")
      if(c.verbose)
        logger.info(srcFile + " ... ")
      
			val htmlContext = new HtmlSrcFileContext(
          srcFile.getAbsolutePath(),
          getPackageInformationForFile(srcFile,info),
          filenamesOriginalToOutputNames)

			val htmlEntries = generateHtmlEntries(info)(htmlContext)

      val outputArray = htmlEntries
			FileUtil.writeToFile(outputFile,htmlContext.getBegin() + outputArray.mkString("") + htmlContext.getEnd())    
	}

  // TODO: Use meta file information.
  private def getPackageInformationForFile(file: File,tokens: scala.collection.mutable.ArrayBuffer[Token]) : String = {
    import org.codeprose.api.ScalaLang._
    val beg = tokens.indexWhere { t => t(tokenType).get == Tokens.PACKAGE }
    var notFound=false
    val packageStr = if(beg != -1){
      val end = tokens.indexWhere({ t => t(tokenType).get == Tokens.WS && t.text.contains("\n")},beg)
      if(end != 1){
        tokens.slice(beg+1, end).map(e=> e.text).mkString.trim()
      } else {
        ""
      }
      
    } else {
      ""
    }
    packageStr
  }
  
	private def generateHtmlEntries(
			infoSorted: scala.collection.mutable.ArrayBuffer[Token])(implicit htmlContext: HtmlSrcFileContext)
  : Iterable[String] = {

    // Group entries into: List[List[Token]] where each List contains a line of text or a MultilineComment
    import org.codeprose.api.ScalaLang._
    
    var idx_toProcess_Beg = 0;
    var idx_toProcess_End = 0;
    var currentLine = 0
    var codeTableOpen = false
    var codeTableClose = false
    val htmlEntries = scala.collection.mutable.ArrayBuffer[String]()
    
    // TODO: Unsave???
    while(idx_toProcess_End<(infoSorted.length-1)){
      
      // Find section to process next
      idx_toProcess_End = determineGroupOfTokensToBeProcessedNext(infoSorted,idx_toProcess_End)
     
      // Update codeTableClose?  
      codeTableClose = updateCodeTableClose(infoSorted,idx_toProcess_End)  
      
      // Process subsection of tokens
      val toProcess = infoSorted.slice(idx_toProcess_Beg, idx_toProcess_End).toArray
      //print("\n------------------\n" + toProcess.map(t=>t.text).mkString(";") )
     
      val (entries,currentLineUpdate,codeTableOpenUpdate) = processGroupsOfTokens(toProcess, currentLine, codeTableOpen, codeTableClose)
      
                 
      htmlEntries+= entries.mkString("\n")
      
      currentLine = currentLineUpdate
      codeTableOpen=codeTableOpenUpdate
      idx_toProcess_Beg = idx_toProcess_End  
    }
    
   // htmlEntries.foreach(t=>println(t))
    
    htmlEntries
    

    
	}

  
  private def determineGroupOfTokensToBeProcessedNext(
      tokens: ArrayBuffer[Token],
      idxLastTokenToProcessedNow: Int) : Int = {
    var idx = idxLastTokenToProcessedNow
    do { idx += 1 }  while(
        idx<tokens.length &&
        tokens(idx)(tokenType).isDefined && 
        tokens(idx)(tokenType).get != Tokens.MULTILINE_COMMENT && 
        !tokens(idx).text.contains("\n"))
      idx
  }
      
  
  private def updateCodeTableClose(tokens: ArrayBuffer[Token], idxLastTokenToProcessedNow: Int) : Boolean = {
    
    val lastIdx = tokens.length-1

    if(idxLastTokenToProcessedNow >= lastIdx){
      true  
    } else {
    	val nextIdx = idxLastTokenToProcessedNow+1
 			val nextToken = tokens(nextIdx)
 			val nextTokenType = nextToken(tokenType)

     if (nextTokenType.isDefined && 
        nextTokenType.get == Tokens.MULTILINE_COMMENT &&
        !CommentUtil.isScalaDocComment(nextToken.text)){ 
      true
    } else {
      false
    }
    }
  }
  
  
  private def processGroupsOfTokens(
      toProcess: Array[Token],
      currentLine: Int,
      codeTableOpen: Boolean,
      codeTableClose: Boolean
      )(implicit htmlContext: HtmlSrcFileContext) : (Array[String],Int,Boolean) = {
    
    var currentLineUpdate = currentLine
    var codeTableOpenUpdate = codeTableOpen
      
    
    
    val entries = if(toTextEntry(toProcess)){
    
      // Close code table and update variables
      codeTableOpenUpdate = false
      
      currentLineUpdate += toProcess(0).text.count(_ == '\n') + 1
      
      val (wrap_beg,wrap_end) = htmlContext.tokenToHtmlEntry.getTokenEntry(toProcess(0))
      if(codeTableOpen){
        Array(htmlContext.codeTable_getEnd() + 
            htmlContext.textTable_getBegin() + 
            htmlContext.textTable_getEntry("", wrap_beg+ wrap_end) + 
            htmlContext.textTable_getEnd())
      } else {
        Array(htmlContext.textTable_getBegin() + 
        htmlContext.textTable_getEntry("", wrap_beg+ wrap_end) + 
        htmlContext.textTable_getEnd())
      }
      
    } else {
     
      
      val (table_beg,table_end) = if(!codeTableOpen && !codeTableClose){
        codeTableOpenUpdate = true
        (htmlContext.codeTable_getBegin(),"")
      } else if (!codeTableOpen && codeTableClose){
        codeTableOpenUpdate = false
        (htmlContext.codeTable_getBegin(), htmlContext.codeTable_getEnd())
      } else if (codeTableOpen && !codeTableClose){
        codeTableOpenUpdate = true
        ("","")
      } else {
        codeTableOpenUpdate = false
        ("",htmlContext.codeTable_getEnd())
      }
      
      // Get html token wrapper
      val wrapper = toProcess.map(t=>{
        htmlContext.tokenToHtmlEntry.getTokenEntry(t)
      })
      
      
     
      
      // Determine (a) all tokens in one line (b) parts of tokens to be split over lines
      val totalLineUpdate = toProcess.map(t=>t.text).mkString("").count(_ == '\n')
      
      val tmp = ArrayBuffer[String]()
      var str = ""
      
      for(i<- 0 until toProcess.length){
        val t = toProcess(i)
        val (beg,end) = wrapper(i)
                     
        if(t.text.contains('\n')){
          
          if(t(tokenType).isDefined && t(tokenType).get == Tokens.WS){
            
    
            
            val numNewLines = t.text.count(_ == '\n')
            val idx_FirstNewLine = t.text.indexOf("\n")
            val idx_LastNewLine = t.text.lastIndexOf("\n")
            //str = str + (beg + t.text.slice(0,idx_FirstNewLine) + "[a]" + end)
            
            //println("WS" + t.offset + " - NL: " + numNewLines + " WSL: " + t.length)
            
            //tmp.append(str)
            str = ""
            
            if(numNewLines>2){
              for(k<- 3 to numNewLines){
                tmp.append("")
              }
            } 
            
            if(idx_LastNewLine < (t.text.length-1)){
              str = (beg + t.text.slice(idx_LastNewLine+1,t.text.length) + end)
            }
            
          } else {
            val splitted = t.text.split('\n')
            
            val numNewLines = t.text.count(_ == '\n')
            val idx_FirstNewLine = t.text.indexOf("\n")
            val idx_LastNewLine = t.text.lastIndexOf("\n")
            
            for (k <- 0 until (splitted.length-1)){
              str = str + (beg + splitted(k) + end)
              tmp.append(str) 
              str = ""
            }
            
            str = (beg + splitted.last + end)
            if(idx_LastNewLine==t.text.length-1){
              tmp.append(str)
              str=""
            }
          }
          
          
        } else {
          str = str + (beg + t.text + end)
        }
        
      }
      
      if(str.length!=0){
        tmp.append(str)
      }
      
      //tmp.foreach(e => print(e))
      val rawEntries = tmp.map( l => {
        val o = htmlContext.codeTable_getEntry(currentLineUpdate, "", l)
        currentLineUpdate+=1
        o
      }).toArray
      
      
      
      val packagedEntries = Array(table_beg) ++ rawEntries ++ Array(table_end)
//      packagedEntries.foreach(e=>println(e))
      packagedEntries
    }
    
    
    //entries.foreach(x=>print(x))
    
    (entries,currentLineUpdate,codeTableOpenUpdate)
    
  }
  
 
  private def toTextEntry(toProcess: Array[Token]) : Boolean = {
   if(toProcess.length==1 && 
       toProcess(0)(tokenType).isDefined &&
       toProcess(0)(tokenType).get == Tokens.MULTILINE_COMMENT && 
       !CommentUtil.isScalaDocComment(toProcess(0).text)
       ){
     true
   } else { 
     false 
   }
  }
  
  
}






trait TokenToOutputEntry {
  val scriptElements = ArrayBuffer[String]()
  def getTokenEntry(token: Token) : (String,String)
  
}

class TokenToOutputEntryHtml(val filenamesOriginalToOutputNames: Array[(String,String)]) extends TokenToOutputEntry {
  def getTokenEntry(token: Token) : (String,String) = {
    ("","")
  }  
}
