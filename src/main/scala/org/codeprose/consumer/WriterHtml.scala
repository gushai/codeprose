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
    val outputMainPath: File,
    val verbose: Boolean
    ) extends ConsumerContext(verbose) {  
  
  val outputRelFolders = List("content","js","style")

  val resourcesToCopy = List[ResourceRelPaths](
      new ResourceRelPaths("/html/style.css","style/style.css"),
      new ResourceRelPaths("/js/codeprose.global.js","js/codeprose.global.js")
      )
  
  val summaryFilesRelPath = Map("summary.index" -> "/index.html",
                                "summary.typeinfo" -> "/typeInformationSummary.html",
                                "summary.whereUsed" -> "/whereUsedSummary.html",
                                "js.global.typeinfo" -> "/js/codeprose.typeinformation.js",
                                "js.global.whereusedinfo" -> "/js/codeprose.whereusedinformation.js")
}



class WriterHtml(implicit c: WriterContextHtml) extends Consumer with LazyLogging {
 
    
	def generateOutput(projectInfo: ProjectInfo) : Unit = {
    
     
     
    setupOutputEnvironment()
    generateGlobalJSInformationFiles(projectInfo.summary)
    val htmlOutputContext = new HtmlOutputContext(c.outputMainPath,projectInfo.enrichedTokens.map(e => e._1).toList)
    generateSummaryPages(projectInfo,htmlOutputContext)
    generateIndividualPages(projectInfo,htmlOutputContext)
    
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
   * Generates and writes to disk the projects summary pages (incl. javascript 'databases') 
   * @param projectInfo ProjectInfor
   * 
   */
  private def generateSummaryPages(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = {
   
    logger.info("Generating summary files ... ")
    generateIndexPage(projectInfo, htmlOutputContext)
    generateWhereUsedPage(projectInfo, htmlOutputContext)
    generateTypeInformationPage(projectInfo, htmlOutputContext)
    
    
    
    
  }
  
  /**
   * Generates and saves the index page to disk.
   * @param projectInfo       Project information
   * @param htmlOutputContext Output context.
   */
  private def generateIndexPage(
      projectInfo: ProjectInfo, 
      htmlOutputContext: HtmlOutputContext) : Unit = { 
    
    val relFileName = c.summaryFilesRelPath.get("summary.index")
    
    if(relFileName.isDefined){
    
      logger.info("\t" + "index file " + relFileName.get)
    
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
         
      val htmlContext = new HtmlSummaryFileContext()      
    
      val srcFilenames = htmlOutputContext.srcFiles
      val labels = htmlOutputContext.filenamesShortened
      val links = htmlOutputContext.outputFilenames
      
      val indexFileTitle = "Overview"
      
      // File listing
      
      val filesHeadline = "<h2>" + "Files" +"</h2>\n"                 
       
      val entries = (srcFilenames zip ( labels zip links)).map{e => (e._1,e._2._1,e._2._2)}
      val filesEntries = entries.map({e => s"""<li>\n<a href="""" + e._3 + s"""" title="Source file:""" + e._1 + s"""">""" + e._2 + s"""</a>\n</li>"""}).mkString("<ul>\n","\n","</ul>\n")
      
      val filesList = filesHeadline + filesEntries
      
      // Classes/Traits/ ... 
      
      val otherHeadline = "<h2>" + "Other..." +"</h2>\n"                 
      val otherEntries = ""     
      
      val otherList = otherHeadline + otherEntries
      
      // TODO Add other information to overview
      
      // create output
      val content = List(filesList,otherList).map(e=>htmlContext.packageContent(e)).mkString("\n\n")  
        
     FileUtil.writeToFile(outputFilename,htmlContext .getBegin(indexFileTitle) + content  + htmlContext .getEnd())      
    } else {
      logger.error("Unable to generate index file. No file name provided!")
    }
    
  }
  
  private def generateTypeInformationPage(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = { 
   
    
    val relFileName = c.summaryFilesRelPath.get("summary.typeinfo")
    
    if(relFileName.isDefined){
    
       logger.info("\t" + "type information \t" + relFileName.get)
    
    } else {
      logger.error("Unable to generate type summary file. No file name provided!")
    }          
    
    
  }
  
  private def generateWhereUsedPage(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = { 

    val relFileName = c.summaryFilesRelPath.get("summary.whereUsed")
    
    if(relFileName.isDefined){
    
       logger.info("\t" + "where used information \t" + relFileName.get)
    
    } else {
      logger.error("Unable to where used summary file. No file name provided!")
    }    
  }
  
 
  
  /**
   * Create the java script files that act as db.
   */
  private def generateGlobalJSInformationFiles(projectSummary: ProjectSummary) : Unit = {
    
    logger.info("Generating js information files ...")
    
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
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "type information: \t\t" + relFileName.get)      

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
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "where used information: \t" + relFileName.get)

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
   * Sets up the output folders and copies resources.
   * 
   */
  private def setupOutputEnvironment() : Unit = {
    val setter = new OutputContextSetter(c.outputMainPath)
    setter.setFolderStructure(c.outputRelFolders)
    c.resourcesToCopy.foreach({ f => setter.copyResource(f.base, new File(c.outputMainPath,f.target)) }) 
  }
  
   /**
   * Generates the output for the individual source files.
   * @param projectInfo ProjectInfo 
   */
  private def generateIndividualPages(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = {
    logger.info("Generating output for the source files ...")  
    
    val files = projectInfo.enrichedTokens.map(e => e._1).toList
    
    implicit val htmlOutputContext = new HtmlOutputContext(c.outputMainPath,files)
    
    
    projectInfo.enrichedTokens.foreach( e => {
      generateSrcFilePage(e._1,e._2,projectInfo.summary,htmlOutputContext)
    })
    
  }
  
  
  /**
   * Generate the output file for an individual source file.
   * @param srcFile    Source file
   * @param tokens  Enriched tokens of the source file.
   * @   TODO
   */
  private def generateSrcFilePage(
      srcFile: File, 
      tokens: ArrayBuffer[Token],
      projectSummary: ProjectSummary,
      htmlOutputContext: HtmlOutputContext) : Unit = {
      
      val srcFileLabel = htmlOutputContext.filenamesOriginalToShortened(srcFile)
      logger.info("\t" + srcFileLabel)

      // Get package name for file
      import org.codeprose.api.ScalaLang._
      val packageName = projectSummary(packageInfoPerFile) match {
        case Some(packagePerFile) => {
          packagePerFile.get(srcFile).getOrElse("")
        }
        case None => { "" }
      }
     
      // Generate output       
      implicit val htmlSrcFileContext = new HtmlSrcFileContext(srcFile.getAbsolutePath,packageName,new TokenToOutputEntryHtml())
      val srcEntries = generateHtmlEntries(tokens).toArray
    
      val outputFile = htmlOutputContext.filenamesOriginalToOutput(srcFile.getAbsolutePath())
      
      FileUtil.writeToFile(outputFile,htmlSrcFileContext.getBegin() + srcEntries.mkString("") + htmlSrcFileContext.getEnd())    

  }

  private def generateHtmlEntries(
      tokens: ArrayBuffer[Token])(implicit htmlContext: HtmlSrcFileContext)
    : Iterable[String] = {
 
    
    
    tokens.map(e=> e.text)
    
    // OLD - Begin
//    // Group entries into: List[List[Token]] where each List contains a line of text or a MultilineComment
//    import org.codeprose.api.ScalaLang._
//    
//    var idx_toProcess_Beg = 0;
//    var idx_toProcess_End = 0;
//    var currentLine = 0
//    var codeTableOpen = false
//    var codeTableClose = false
//    val htmlEntries = scala.collection.mutable.ArrayBuffer[String]()
//    
//    // TODO: Unsave???
//    while(idx_toProcess_End<(infoSorted.length-1)){
//      
//      // Find section to process next
//      idx_toProcess_End = determineGroupOfTokensToBeProcessedNext(infoSorted,idx_toProcess_End)
//     
//      // Update codeTableClose?  
//      codeTableClose = updateCodeTableClose(infoSorted,idx_toProcess_End)  
//      
//      // Process subsection of tokens
//      val toProcess = infoSorted.slice(idx_toProcess_Beg, idx_toProcess_End).toArray
//      //print("\n------------------\n" + toProcess.map(t=>t.text).mkString(";") )
//     
//      val (entries,currentLineUpdate,codeTableOpenUpdate) = processGroupsOfTokens(toProcess, currentLine, codeTableOpen, codeTableClose)
//      
//                 
//      htmlEntries+= entries.mkString("\n")
//      
//      currentLine = currentLineUpdate
//      codeTableOpen=codeTableOpenUpdate
//      idx_toProcess_Beg = idx_toProcess_End  
//    }
//    
//   // htmlEntries.foreach(t=>println(t))
//    
//    htmlEntries
    // OLD - End
    
    
  }
  
  
  
  // OLD - BEGIN
  // --------------------------------------------------------------------------------------
  
  
//	private def generateIndexFile(
//      originalFilenames: List[String], 
//      filenamesShortened : List[String], 
//      links: List[String]
//      ) : Unit = {   
//			val outputFilename= new File(c.outputMainPath.getAbsolutePath + "/index.html")
//      logger.info("Index page: \t" + outputFilename + " ...")      
//			val htmlFrame = new HtmlSummaryFileContext()			
//			FileUtil.writeToFile(outputFilename,htmlFrame.begin + htmlFrame.getFileListing(originalFilenames,filenamesShortened,links) + htmlFrame.end)      
//	} 
//
//	private def generateOutputFile(
//			outputFile: File, 
//			srcFile: File, 
//			info: scala.collection.mutable.ArrayBuffer[Token],
//      filenamesOriginalToOutputNames : Array[(String,String)]
//      ) : Unit = {
//
//      logger.info("Individual pages ... ")
//      if(c.verbose)
//        logger.info(srcFile + " ... ")
//      
//			val htmlContext = new HtmlSrcFileContext(
//          srcFile.getAbsolutePath(),
//          getPackageInformationForFile(srcFile,info),
//          filenamesOriginalToOutputNames)
//
//			val htmlEntries = generateHtmlEntries(info)(htmlContext)
//
//      val outputArray = htmlEntries
//			FileUtil.writeToFile(outputFile,htmlContext.getBegin() + outputArray.mkString("") + htmlContext.getEnd())    
//	}
//
//  // TODO: Use meta file information.
//  private def getPackageInformationForFile(file: File,tokens: scala.collection.mutable.ArrayBuffer[Token]) : String = {
//    import org.codeprose.api.ScalaLang._
//    val beg = tokens.indexWhere { t => t(tokenType).get == Tokens.PACKAGE }
//    var notFound=false
//    val packageStr = if(beg != -1){
//      val end = tokens.indexWhere({ t => t(tokenType).get == Tokens.WS && t.text.contains("\n")},beg)
//      if(end != 1){
//        tokens.slice(beg+1, end).map(e=> e.text).mkString.trim()
//      } else {
//        ""
//      }
//      
//    } else {
//      ""
//    }
//    packageStr
//  }
//  
//	private def generateHtmlEntries(
//			infoSorted: scala.collection.mutable.ArrayBuffer[Token])(implicit htmlContext: HtmlSrcFileContext)
//  : Iterable[String] = {
//
//    // Group entries into: List[List[Token]] where each List contains a line of text or a MultilineComment
//    import org.codeprose.api.ScalaLang._
//    
//    var idx_toProcess_Beg = 0;
//    var idx_toProcess_End = 0;
//    var currentLine = 0
//    var codeTableOpen = false
//    var codeTableClose = false
//    val htmlEntries = scala.collection.mutable.ArrayBuffer[String]()
//    
//    // TODO: Unsave???
//    while(idx_toProcess_End<(infoSorted.length-1)){
//      
//      // Find section to process next
//      idx_toProcess_End = determineGroupOfTokensToBeProcessedNext(infoSorted,idx_toProcess_End)
//     
//      // Update codeTableClose?  
//      codeTableClose = updateCodeTableClose(infoSorted,idx_toProcess_End)  
//      
//      // Process subsection of tokens
//      val toProcess = infoSorted.slice(idx_toProcess_Beg, idx_toProcess_End).toArray
//      //print("\n------------------\n" + toProcess.map(t=>t.text).mkString(";") )
//     
//      val (entries,currentLineUpdate,codeTableOpenUpdate) = processGroupsOfTokens(toProcess, currentLine, codeTableOpen, codeTableClose)
//      
//                 
//      htmlEntries+= entries.mkString("\n")
//      
//      currentLine = currentLineUpdate
//      codeTableOpen=codeTableOpenUpdate
//      idx_toProcess_Beg = idx_toProcess_End  
//    }
//    
//   // htmlEntries.foreach(t=>println(t))
//    
//    htmlEntries
//    
//
//    
//	}
//
//  
//  private def determineGroupOfTokensToBeProcessedNext(
//      tokens: ArrayBuffer[Token],
//      idxLastTokenToProcessedNow: Int) : Int = {
//    var idx = idxLastTokenToProcessedNow
//    do { idx += 1 }  while(
//        idx<tokens.length &&
//        tokens(idx)(tokenType).isDefined && 
//        tokens(idx)(tokenType).get != Tokens.MULTILINE_COMMENT && 
//        !tokens(idx).text.contains("\n"))
//      idx
//  }
//      
//  
//  private def updateCodeTableClose(tokens: ArrayBuffer[Token], idxLastTokenToProcessedNow: Int) : Boolean = {
//    
//    val lastIdx = tokens.length-1
//
//    if(idxLastTokenToProcessedNow >= lastIdx){
//      true  
//    } else {
//    	val nextIdx = idxLastTokenToProcessedNow+1
// 			val nextToken = tokens(nextIdx)
// 			val nextTokenType = nextToken(tokenType)
//
//     if (nextTokenType.isDefined && 
//        nextTokenType.get == Tokens.MULTILINE_COMMENT &&
//        !CommentUtil.isScalaDocComment(nextToken.text)){ 
//      true
//    } else {
//      false
//    }
//    }
//  }
//  
//  
//  private def processGroupsOfTokens(
//      toProcess: Array[Token],
//      currentLine: Int,
//      codeTableOpen: Boolean,
//      codeTableClose: Boolean
//      )(implicit htmlContext: HtmlSrcFileContext) : (Array[String],Int,Boolean) = {
//    
//    var currentLineUpdate = currentLine
//    var codeTableOpenUpdate = codeTableOpen
//      
//    
//    
//    val entries = if(toTextEntry(toProcess)){
//    
//      // Close code table and update variables
//      codeTableOpenUpdate = false
//      
//      currentLineUpdate += toProcess(0).text.count(_ == '\n') + 1
//      
//      val (wrap_beg,wrap_end) = htmlContext.tokenToHtmlEntry.getTokenEntry(toProcess(0))
//      if(codeTableOpen){
//        Array(htmlContext.codeTable_getEnd() + 
//            htmlContext.textTable_getBegin() + 
//            htmlContext.textTable_getEntry("", wrap_beg+ wrap_end) + 
//            htmlContext.textTable_getEnd())
//      } else {
//        Array(htmlContext.textTable_getBegin() + 
//        htmlContext.textTable_getEntry("", wrap_beg+ wrap_end) + 
//        htmlContext.textTable_getEnd())
//      }
//      
//    } else {
//     
//      
//      val (table_beg,table_end) = if(!codeTableOpen && !codeTableClose){
//        codeTableOpenUpdate = true
//        (htmlContext.codeTable_getBegin(),"")
//      } else if (!codeTableOpen && codeTableClose){
//        codeTableOpenUpdate = false
//        (htmlContext.codeTable_getBegin(), htmlContext.codeTable_getEnd())
//      } else if (codeTableOpen && !codeTableClose){
//        codeTableOpenUpdate = true
//        ("","")
//      } else {
//        codeTableOpenUpdate = false
//        ("",htmlContext.codeTable_getEnd())
//      }
//      
//      // Get html token wrapper
//      val wrapper = toProcess.map(t=>{
//        htmlContext.tokenToHtmlEntry.getTokenEntry(t)
//      })
//      
//      
//     
//      
//      // Determine (a) all tokens in one line (b) parts of tokens to be split over lines
//      val totalLineUpdate = toProcess.map(t=>t.text).mkString("").count(_ == '\n')
//      
//      val tmp = ArrayBuffer[String]()
//      var str = ""
//      
//      for(i<- 0 until toProcess.length){
//        val t = toProcess(i)
//        val (beg,end) = wrapper(i)
//                     
//        if(t.text.contains('\n')){
//          
//          if(t(tokenType).isDefined && t(tokenType).get == Tokens.WS){
//            
//    
//            
//            val numNewLines = t.text.count(_ == '\n')
//            val idx_FirstNewLine = t.text.indexOf("\n")
//            val idx_LastNewLine = t.text.lastIndexOf("\n")
//            //str = str + (beg + t.text.slice(0,idx_FirstNewLine) + "[a]" + end)
//            
//            //println("WS" + t.offset + " - NL: " + numNewLines + " WSL: " + t.length)
//            
//            //tmp.append(str)
//            str = ""
//            
//            if(numNewLines>2){
//              for(k<- 3 to numNewLines){
//                tmp.append("")
//              }
//            } 
//            
//            if(idx_LastNewLine < (t.text.length-1)){
//              str = (beg + t.text.slice(idx_LastNewLine+1,t.text.length) + end)
//            }
//            
//          } else {
//            val splitted = t.text.split('\n')
//            
//            val numNewLines = t.text.count(_ == '\n')
//            val idx_FirstNewLine = t.text.indexOf("\n")
//            val idx_LastNewLine = t.text.lastIndexOf("\n")
//            
//            for (k <- 0 until (splitted.length-1)){
//              str = str + (beg + splitted(k) + end)
//              tmp.append(str) 
//              str = ""
//            }
//            
//            str = (beg + splitted.last + end)
//            if(idx_LastNewLine==t.text.length-1){
//              tmp.append(str)
//              str=""
//            }
//          }
//          
//          
//        } else {
//          str = str + (beg + t.text + end)
//        }
//        
//      }
//      
//      if(str.length!=0){
//        tmp.append(str)
//      }
//      
//      //tmp.foreach(e => print(e))
//      val rawEntries = tmp.map( l => {
//        val o = htmlContext.codeTable_getEntry(currentLineUpdate, "", l)
//        currentLineUpdate+=1
//        o
//      }).toArray
//      
//      
//      
//      val packagedEntries = Array(table_beg) ++ rawEntries ++ Array(table_end)
////      packagedEntries.foreach(e=>println(e))
//      packagedEntries
//    }
//    
//    
//    //entries.foreach(x=>print(x))
//    
//    (entries,currentLineUpdate,codeTableOpenUpdate)
//    
//  }
//  
// 
//  private def toTextEntry(toProcess: Array[Token]) : Boolean = {
//   if(toProcess.length==1 && 
//       toProcess(0)(tokenType).isDefined &&
//       toProcess(0)(tokenType).get == Tokens.MULTILINE_COMMENT && 
//       !CommentUtil.isScalaDocComment(toProcess(0).text)
//       ){
//     true
//   } else { 
//     false 
//   }
//  }
  
  
}




