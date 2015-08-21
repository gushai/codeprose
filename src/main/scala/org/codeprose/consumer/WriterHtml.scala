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
import org.codeprose.api.ERangePositionWithTokenId


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
                                "summary.packageinfo" -> "/packageSummary.html",
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
    generatePackageInformationPage(projectInfo, htmlOutputContext)
     
        
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
        
     FileUtil.writeToFile(outputFilename,htmlContext.getBegin(indexFileTitle) + content  + htmlContext.getEnd())      
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
  
  private def generatePackageInformationPage(projectInfo: ProjectInfo, htmlOutputContext: HtmlOutputContext) : Unit = { 

    val relFileName = c.summaryFilesRelPath.get("summary.packageinfo")
    
    if(relFileName.isDefined){
    
       logger.info("\t" + "package information \t" + relFileName.get)
    
    } else {
      logger.error("Unable to package summary file. No file name provided!")
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
   * 
   * Functions:
   *  - getTypeIds()  returns a list of all typeIds found
   *  - typeInformation(typeId) returns TypeInformation or null
   * 
   * @param typeInfos Type information by type id.
   */
  private def generateGlobalJSTypeInfo(typeInfos: Map[Int,Option[TypeInformation]]) : Unit = {
    
    val relFileName = c.summaryFilesRelPath.get("js.global.typeinfo")
    
    if(relFileName.isDefined){
      
      val outputFilename= new File(c.outputMainPath.getAbsolutePath + relFileName.get)
      logger.info("\t" + "type information: \t\t" + relFileName.get)      

      // Type ids
      val begTypeId = s"""
// Type Ids
function getTypeIds(){\n"""
      val entriesTypeId = s"""\treturn """ + typeInfos.map(e => e._1).toList.sorted.toJson.compactPrint +";"
      val endTypeId = "\n};"
      
      val contentTypeId = begTypeId + entriesTypeId + endTypeId
      
      // Type information
      
      val begTypeInfo = s"""
// type information
function typeInformation(typeId){ 
  tInfo = null;
  switch(typeId){
"""
        
      val endTypeInfo = s"""
  default: \n\t\t tInfo=null;
  }
  return tInfo;
};"""
      
      val entriesTypeInfo = typeInfos.map(e => {
        val typeId = e._1
        e._2 match {
          case Some(tI) => {            
            val jsonStr = tI.toJson.compactPrint
            s"""\tcase $typeId:\n\t\ttInfo=""" + jsonStr + s"""; break;"""
          } 
          case None => {""}
        }
        
      }).mkString("\n")
      
      val contentTypeInfo = begTypeInfo + entriesTypeInfo + endTypeInfo
      
      // Generate output      
      val content = List(contentTypeId,contentTypeInfo)
      FileUtil.writeToFile(outputFilename,content.mkString("\n\n"))      

    } else {
      logger.error("Unable to generate js file with type information. No file name provided!")
    }
    
  }
  
  
  
  
  
  /**
   * Saves where used information to disk in js file.
   * @param whereUsed Source positions by type id.
   */
  private def generateGlobalJSWhereUsedInfo(whereUsed: Map[Int,List[ERangePositionWithTokenId]]) : Unit = {
    
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
      val packageName = projectSummary(packageNamePerFile) match {
        case Some(packagePerFile) => {
          packagePerFile.get(srcFile).getOrElse("")
        }
        case None => { "" }
      }
     
      // Generate output       
      implicit val htmlSrcFileContext = new HtmlSrcFileContext(
          srcFile.getAbsolutePath,
          packageName,
          new TokenToOutputEntryHtml()(htmlOutputContext))
      val srcEntries = generateHtmlEntries(tokens).toArray
    
      val outputFile = htmlOutputContext.filenamesOriginalToOutput(srcFile.getAbsolutePath())
      
      FileUtil.writeToFile(outputFile,htmlSrcFileContext.getBegin() + srcEntries.mkString("") + htmlSrcFileContext.getEnd())    

  }

  private def generateHtmlEntries(
      tokens: ArrayBuffer[Token])(implicit htmlContext: HtmlSrcFileContext)
    : Iterable[String] = {
 
    
    
    tokens.map(e=> e.text)
    
    // Group entries into: List[List[Token]] where each List contains a line of src code or a MultilineComment
    import org.codeprose.api.ScalaLang._
    
    var idx_toProcess_Beg = 0;
    var idx_toProcess_End = 0;
    var currentLine = 0
    var codeTableOpen = false   // true: a code table environment is open
    var codeTableClose = false  // true: close code table environment
    val htmlEntries = scala.collection.mutable.ArrayBuffer[String]()

    // TODO: Unsave???
    while(idx_toProcess_End<(tokens.length-1)){
      
      // Find section to process next
      idx_toProcess_End = determineGroupOfTokensToBeProcessedNext(tokens,idx_toProcess_End)
     
      // Update codeTableClose?  
      codeTableClose = updateCodeTableClose(tokens,idx_toProcess_End)  
      
      // Process subsection of tokens
      val toProcess = tokens.slice(idx_toProcess_Beg, idx_toProcess_End).toArray
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
  
  /**
   * Determines a group of tokens to be processed together.
   * @param tokens                      Tokens.
   * @param idxLastTokenToProcessedNow  Last used index.
   * @return                            New index.
   */
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
  
  /**
   * Processes group of tokens and returns their html output,
   * the line update and an indicator for an open code table.
   * @param toProcess       Tokens to process.
   * @param currentLine     Current line number.
   * @param codeTableOpen   Indicator true if code table open.
   * @param codeTableClose  Indicator true if code table has to be closed.
   * @return                Html entries, line number update, code table open indicator.
   */
  private def processGroupsOfTokens(
      toProcess: Array[Token],
      currentLine: Int,
      codeTableOpen: Boolean,
      codeTableClose: Boolean
      )(implicit htmlContext: HtmlSrcFileContext) : (Array[String],Int,Boolean) = {
    
    var currentLineUpdate = currentLine
    var codeTableOpenUpdate = codeTableOpen
      
    // DEBUG
    // Print the text from the grouped tokens
    // println(toProcess.map(e=>e.text).mkString(";") +"\n-------------------------------------------------------")
    
    // IDEA:     return WS Strig as indentations for the next section!!! 
    // PROBLEM:  Issue that WS with \n ends Group to process but has WS for next line indentation!!
    
    // TEXT ENTRY
    val entries = if(toTextEntry(toProcess)){
    
      // Close code table and update variables
      codeTableOpenUpdate = false
      
      currentLineUpdate += toProcess(0).text.count(_ == '\n') + 1
      
      // Text
      val (wrap_beg,wrap_end) = htmlContext.tokenToOutputEntry.getTokenEntryWrapper(toProcess(0))
      
      
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
      
    // CODE ENTRY
    } else {
     
      // Determine code section surrounding
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
        htmlContext.tokenToOutputEntry.getTokenEntryWrapper(t)
      })
      
           
      // Determine (a) all tokens in one line (b) parts of tokens to be split over lines
      val totalLineUpdate = toProcess.map(t=>t.text).mkString("").count(_ == '\n')
      
      val tmp = ArrayBuffer[String]()
      var str = ""
      
      // Process tokens
      for(i <- 0 until toProcess.length){
        val t = toProcess(i)
        val (beg,end) = wrapper(i)
            
        // Token text with '\n'
        if(t.text.contains('\n')){
          
          // WS token          
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
          // Token w/ \n, but not WS  
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
          
        // Tokens w/o '\n'  
        } else {
          str = str + (beg + t.text + end)
        }
        
      }
      
      if(str.length!=0){
        tmp.append(str)
      }
      
      //tmp.foreach(e => print(e))
      val rawEntries = tmp.map( l => {
        val o = htmlContext.codeTable_getEntry("",currentLineUpdate, l)
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
  
  
  /**
   * Returns indicator whether code table needs to be closed.
   * @param tokens                      All tokens of a file.
   * @param idxLastTokenToProcessedNow  Index of last token in current group to be processed.
   * @return                            Boolean indicator. 
   */
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

  /**
   * Checks if tokens have to processed as text.
   * @param toProcess Tokens to process.
   * @return          Boolean indicator. 
   */
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




