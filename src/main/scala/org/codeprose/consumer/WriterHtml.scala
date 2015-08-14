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
import org.codeprose.api.TokenProperties._



class ResourceRelPaths(val base: String, val target: String)

trait WriterContext {
  val verbose: Boolean
  
}

class WriterContextHtml(
    val verbose: Boolean
    ) extends WriterContext {
  
  val outputFolders = List("content","js","style")
  val styleSheetRelPath = new ResourceRelPaths("/html/style.css","style/style.css") 
  val filesToCoy = List[ResourceRelPaths](styleSheetRelPath)
}



class WriterHtml(outputPath: File)(implicit c: WriterContextHtml)
extends Consumer with LazyLogging {
 
  
	def generateOutput(
			info: scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[Token])]
			): Unit = {
    
      import org.codeprose.util.StringUtil
			val filenamesShorted = StringUtil.getUniqueShortFileNames(info.map(e => e._1.getAbsolutePath).toList)
			val outputFilenames = filenamesShorted.map(s => outputPath + "/content/" + s.replace("/","_") + ".html")
      
      val filenamesOriginalToOutput =  info.map(e => e._1.getAbsolutePath).zip(outputFilenames).toArray
      
      logger.info("Generating output ...")					

      // Output context
      setupOutputContext(outputPath)
      
      generateIndexFile(info.map(e => e._1.getAbsolutePath()).toList,filenamesShorted,outputFilenames)
      
			var idx=0
			for(i<-info){     
			  generateOutputFile(new File(outputFilenames(idx)),i._1,i._2,filenamesOriginalToOutput)
				idx+=1
      }
			logger.info("Done.")
	}

	private def generateIndexFile(
      originalFilenames: List[String], 
      filenamesShortened : List[String], 
      links: List[String]
      ) : Unit = {   
			val outputFilename= new File(outputPath.getAbsolutePath + "/index.html")
      logger.info("Index page: \t" + outputFilename + " ...")      
			val htmlFrame = new HtmlIndexContext()			
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
      
			val htmlContext = new HtmlContext(
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
			infoSorted: scala.collection.mutable.ArrayBuffer[Token])(implicit htmlContext: HtmlContext)
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
      )(implicit htmlContext: HtmlContext) : (Array[String],Int,Boolean) = {
    
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
  
  private def setupOutputContext(outputPath: File) : Unit = {
    val setter = new OutputContextSetter(outputPath)
    setter.setFolderStructure(c.outputFolders)
    c.filesToCoy.foreach({ f => setter.copyResource(f.base, new File(outputPath,f.target)) }) 
	}
}





class HtmlContext(filename: String, packag: String, 
    val filenamesOriginalToOutputNames : Array[(String,String)]) {

    val tokenToHtmlEntry = new TokenToOutputEntry(filenamesOriginalToOutputNames)

  
	    val fileNameWithoutPath =  filename.slice(filename.lastIndexOf("/")+1, filename.length())  
			val fileNameWithoutPathAndEnding = fileNameWithoutPath.slice(0,fileNameWithoutPath.lastIndexOf("."))		
			val fileNameEnding = fileNameWithoutPath.slice(fileNameWithoutPath.lastIndexOf(".")+1,fileNameWithoutPath.length)

      
			def getBegin() : String = {
      
      val perTokenScripts = tokenToHtmlEntry.scriptElements.mkString("\n", "\n\n", "\n")
      
      
      val perFileScripts = List(
      s"""
        // Highlight implicit conversions and parameters
      function highlightImplicitConversionsAndParameters(){

        // Implicit conversions
        $$('*[data-cp-implicitconversion=true]').toggleClass("highlightImplicitConversion");

        // Implicit parameters
        // Color to use highlightImplicitParameter
      }""",
      s"""   
      $$(document).keypress(function(e){
        // i
        if(e.keyCode == 105){
          highlightImplicitConversionsAndParameters()
        }     
      });"""
      ).mkString("\n", "\n\n", "\n")
      
      
      s"""<!doctype HTML>
			<html lang="en">
			<head>
			<meta http-equiv="content-type" content="text/html; charset=utf-8" />
			<link rel="stylesheet" type="text/css" href="../style/style.css" media="screen" />
			<title>$fileNameWithoutPath</title>
      <script src="https://code.jquery.com/jquery-1.10.2.js"></script>
			<script type="text/javascript">
      $$(document).ready(function(){ """ +
      perFileScripts +
      perTokenScripts + 
      s"""\n});
			</script>
			</head>
			<body>
			<div align="center">
			<div class="header" id="header" align="center"> 
			<div style="float:left;"> 
			<span style="font-size:1.5em;font-weight:bold;" title="$filename">$fileNameWithoutPath&nbsp;</span><span style="font-size:1em;">$packag</span></div>
			<div style="float:right;"><a style="color:black;font-weight:bold;" href="../index.html">overview</a></div>
			</div>
			<div class="content">\n"""
      }
			
      def getEnd() : String  = {
        s"""
          
      <div class="footer">generated by codeprose. help and support on <a href="http://github.com/sth/codeprose" target="blank">github</a>.</div>
			</div></div></body></html>"""
        }

      
      def textTable_getBegin() : String = { s"""<table class="table-text">""" } 

      def textTable_getEnd() : String = { s"""</table>\n\n""" } 
      
      def textTable_getEntry(comment: String, mainText: String) : String = {
        s"""<tr class="table-text-line">
<td class="table-text-comment">$comment</td>
<td class="table-text-text">$mainText</td>
</tr>"""
      } 

      def codeTable_getBegin() : String = { """<table class="table-code">""" }
      
      
      def codeTable_getEntry(lineNumber: Int, comment: String, code: String) : String = {
        val lineNumStr = lineNumber.toString()
        s"""<tr id="LCONT$lineNumStr" class="table-code-line" >
<td id="LCOM$lineNumStr" class="table-code-comment">$comment</td>
<td id="L$lineNumStr" class="table-code-linenumber" data-line-number="$lineNumStr">$lineNumStr</td>
<td id="LC$lineNumStr" class="table-code-code">
<pre>$code</pre>
</td>
</tr>""" 
      }
      
      def codeTable_getEnd() : String = { s"""</table>\n\n""" }
      
}

class HtmlIndexContext(){

	val begin =s"""<!doctype HTML>
			<html lang="en">
			<head>
			<meta http-equiv="content-type" content="text/html; charset=utf-8" />
			<link rel="stylesheet" type="text/css" href="./style/style.css" media="screen" />
			<title>Overview</title>
      <script src="https://code.jquery.com/jquery-1.10.2.js"></script>
			<script type="text/javascript">
			</script>
			</head>
			<body>
			<div align="center">		
			<div class="header" id="header"> 
			<div style="text-align:left;"> 
			<span style="font-size:1.5em;font-weight:bold;">Overview&nbsp;</span></div>
      <div style="text-align:right;"></div>			
			</div>
			<div class="content">
			"""

			val end  = s"""<div class="footer">generated by codeprose. help and support on <a href="http://github.com/sth/codeprose" target="blank">github</a>.</div>
			</div></div></body></html>"""

			def getFileListing(
          originalFilenames: List[String],
          labels: List[String], 
          links: List[String]
          ): String = {
			
      // TODO group based on folders
			val frameBeg = s"""<div class="textbox">"""
			val frameEnd = s"""</div>\n"""
			val beg = "<h2>" + "Files" +"</h2>"                 
			
      
      val dataAttributesPrefix = "data-cp-"
      
      var entries = (originalFilenames zip ( labels zip links)).map{e => (e._1,e._2._1,e._2._2)}

			frameBeg+ beg + "<ul>" + entries.map({e => 
			s"""<li><a href="""" + 
			e._3 + s"""" title="Originial filename:""" +e._1 + s"""">""" +
			e._2 + s"""</a></li>"""}).mkString("\n") + "</ul>\n" + frameEnd
      
      
	}

}


class TokenToOutputEntry(val filenamesOriginalToOutputNames: Array[(String,String)]){
  
   val scriptElements = ArrayBuffer[String]()
  
  
   private def getTokenEntry_WithInformation(token: Token) : (String,String) = {
    val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
    (s"""<span title=""""+ tInfo + s"""">""",s"""</span>""")
   }
  
   private def getTokenEntry_WithOutInformation(token: Token) : String = {
     token.text
  }
   
   def getTokenEntry(token: Token) : (String,String) = {
   
     import org.codeprose.api.ScalaLang._
     
     val out = token(tokenType) match {
       case Some(tt) => {
          val text = if(tt.isKeyword){
            handleKeywords(token,tt)             
          } else if(tt.isLiteral) {
            handleLiterals(token,tt)
          } else if(tt.isComment) {
            handleComments(token,tt)
          } else if(tt.isId){
            handleIds(token,tt)
          }
          else {        
            
            tt match {
              case Tokens.WS => {
                handleWS(token)
              }
              case _ => {
                ("","")
              }
            }
            
          }
          text
       }
       case None => {
         ("","")
       }
       
     }
     out 
   }
 

   private def handleComments(token: Token, tt: ScalaTokenType) : (String,String) = {
		   import org.codeprose.consumer.util.MarkdownConverter
		   import org.codeprose.consumer.util.CommentUtil


		   tt match{ 
		   case Tokens.MULTILINE_COMMENT => {
			   val s = if(CommentUtil.isScalaDocComment(token.text)){
				   (s"""<span class="scaladoc">""","</span>")
			   } else {
				   (MarkdownConverter.apply(CommentUtil.cleanMultilineComment(token.text)),"") 
			   }
			   s
		   }        
		   case _ => {          
			   //val tInfo = token.toString().replace(";",";\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
				 //	   s"""<span class="comment" title="""" + tInfo + s"""">""" + token.text + "</span>"
         (s"""<span class="comment">""", "</span>")
		   }        
		   }
   }
  
  
   /**
    * 
    */
   private def handleKeywords(token: Token, tt: ScalaTokenType) : (String,String) = {
      import org.codeprose.api.ScalaLang.Tokens._
      
      val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
    
      
      tt match {
      case ABSTRACT => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case CASE => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case CATCH => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case CLASS => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case DEF => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case DO => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case ELSE => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case EXTENDS => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case FINAL => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case FINALLY => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case FOR => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case FORSOME => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case IF => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case IMPLICIT => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case IMPORT => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case LAZY => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case MATCH => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case NEW => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case OBJECT => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case OVERRIDE => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case PACKAGE => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case PRIVATE => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case PROTECTED => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case RETURN => {
        (s"""<span class="return" title="$tInfo">""", "</span>") 
      }
      case SEALED => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case SUPER => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case THIS => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case THROW => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case TRAIT => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case TRY => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case TYPE => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case VAL => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case VAR => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case WHILE => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case WITH => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>") 
      }
      case YIELD => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>")
      }
      case _ => {
        (s"""<span class="keyword" title="$tInfo">""", "</span>")
      }
     }
   }
   private def handleLiterals(token: Token, tt: ScalaTokenType) : (String,String) = {
    import org.codeprose.api.ScalaLang.Tokens._
    
    val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
    
    val dataAttributes = getHtmlDataAttributes(token,"data-cp-").map(e=> e._1 + "=" + e._2).mkString(" "," "," ")
    
    
    tt match {
    case CHARACTER_LITERAL => {
      (s"""<span class="stringLiteral" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case INTEGER_LITERAL => {
      (s"""<span class="numberLiteral" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case FLOATING_POINT_LITERAL => {
      (s"""<span class="numberLiteral" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case STRING_LITERAL => {
      (s"""<span class="stringLiteral" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case STRING_PART => {
      (s"""<span class="stringLiteral" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case SYMBOL_LITERAL => {
      (s"""<span class="literal" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case TRUE => {
      (s"""<span class="keyword" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case FALSE => {
      (s"""<span class="keyword" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }
    case NULL => {
      (s"""<span class="keyword" $dataAttributes title="""" + tInfo +s"""">""", "</span>")
    }

    }
   }
   
   private def handleIds(token: Token, tt: ScalaTokenType) : (String,String) = {
    
     val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
     val titleElem = s""" title="$tInfo" """
     
     
     // Script element for highlight where used within file
     token(internalTokenId) match {
       case Some(id) => {
         
         scriptElements.append(
             s"""$$("#T""" + id + s"""").hover( 
          function(){ toHighlight = $$(this).data("cp-whereusedinfile"); $$(toHighlight).css("background-color","yellow"); },
          function(){ toHighlight = $$(this).data("cp-whereusedinfile"); $$(toHighlight).css("background-color","#F8F8F8"); }
          );""")
       }
       case None => {}
     }
     
     
     
     
     
     
     import org.codeprose.api.ScalaLang.Tokens._ 
     
     tt match {
       case VARID => {
           handleVARID(token)
         } 
        case PLUS => {
          hanldeID(token)
         } 
        case MINUS => {
          hanldeID(token)
         } 
        case STAR => {
          hanldeID(token)
         } 
        case PIPE => {
          hanldeID(token)
         } 
        case TILDE => {
          hanldeID(token)
         } 
        case EXCLAMATION => {
          hanldeID(token)
         } 
         case _ => {
           (s"""<span $titleElem>""","</span>")
         }
     }
   }
   
   
  private def handleVARID(token: Token) : (String,String) = {

		// Fill title information
		val tInfo = token.toString().replace(";", ";\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
    
    val iTIdHtmlElement = token(internalTokenId) match {
      case Some(id) => {s""" id="T$id" """}
      case None => s""" """
    }
        
    val (linkToDeclaredAt_beg,linkToDeclaredAt_end) = if(token(declaredAt_TokenIdSrcPos).isDefined){
      
      val srcPos = token(declaredAt_TokenIdSrcPos).get
      
      // Get full file to output translation
      val outputFileRelPath = filenamesOriginalToOutputNames.filter( e => e._1 == srcPos.filename).map(e => "./" + e._2)
      if(outputFileRelPath.length>0){
        val tId = srcPos.tokenId
        val idx = outputFileRelPath(0).lastIndexOf("/")
        val link = "." + outputFileRelPath(0).slice(idx,outputFileRelPath(0).length) + "#" + "T" + tId.toString
        (s"""<a href="$link" class="in-code">""","</a>")
      } else {
        ("","")
      }
    } else { ("","") }
    
    val spanClass = token(symbolDesignation) match {
      case Some(spClass) => {s""" class="$spClass" """}
      case None => " "
    }
    
    // Data attributes 
    val dataAttributes = getHtmlDataAttributes(token,"data-cp-").map(e=> e._1 + "=" + e._2).mkString(" "," "," ")
    
    // Set output 
    val spanElementBeg = s"""<span""" + spanClass + iTIdHtmlElement + dataAttributes + s""" title="$tInfo">"""
    val spanElementEnd = "</span>"
      
    if(linkToDeclaredAt_beg.length()!=0){
      (linkToDeclaredAt_beg + spanElementBeg, spanElementEnd + linkToDeclaredAt_end)
    } else {
      (spanElementBeg,spanElementEnd)
    }
		
   }
   
   private def hanldeID(token: Token) : (String, String) = {
     // Fill title information
    val tInfo = token.toString().replace(";", ";\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
    
    val iTIdHtmlElement = token(internalTokenId) match {
      case Some(id) => {s""" id="T$id" """}
      case None => s""" """
    }
    // Link elements to declared_At   
    val (linkToDeclaredAt_beg,linkToDeclaredAt_end) = if(token(declaredAt_TokenIdSrcPos).isDefined){
      
      val srcPos = token(declaredAt_TokenIdSrcPos).get
      
      // Get full file to output translation
      val outputFileRelPath = filenamesOriginalToOutputNames.filter( e => e._1 == srcPos.filename).map(e => "./" + e._2)
      if(outputFileRelPath.length>0){
        val tId = srcPos.tokenId
        val idx = outputFileRelPath(0).lastIndexOf("/")
        val link = "." + outputFileRelPath(0).slice(idx,outputFileRelPath(0).length) + "#" + "T" + tId.toString
        (s"""<a href="$link" class="in-code">""","</a>")
      } else {
        ("","")
      }
    } else { ("","") }
    
    val spanClass = token(symbolDesignation) match {
      case Some(spClass) => {s""" class="$spClass" """}
      case None => " "
    }
    
   
        
     // Data attributes 
    val dataAttributes = getHtmlDataAttributes(token,"data-cp-").map(e=> e._1 + "=" + e._2).mkString(" "," "," ")
    
    // Set output 
    val spanElementBeg = s"""<span""" + spanClass + iTIdHtmlElement + dataAttributes + s""" title="$tInfo">"""
    val spanElementEnd = "</span>"
      
    if(linkToDeclaredAt_beg.length()!=0){
      (linkToDeclaredAt_beg + spanElementBeg, spanElementEnd + linkToDeclaredAt_end)
    } else {
      (spanElementBeg,spanElementEnd)
    }
    
   }
  
   private def handleWS(token: Token) : (String,String) = {
     ("","")
   }
   
   private def getHtmlDataAttributes(token: Token, dataAttributesPrefix: String) : ArrayBuffer[(String,String)] = {
     
     
     
     token(tokenType) match {
       case Some(tt) => {
         
         if(tt.isId){
           getHtmlDataAttributesIds(token, dataAttributesPrefix)
         } else if (tt.isKeyword) {
           ArrayBuffer[(String,String)]()
         } else if (tt.isXml) {
           ArrayBuffer[(String,String)]()
         } else if (tt.isComment){
           ArrayBuffer[(String,String)]()
         } else if (tt.isLiteral) {
           getHtmlDataAttributesLiterals(token, dataAttributesPrefix)
         } else {
           ArrayBuffer[(String,String)]()
         }
         
       }
       case None => {
         ArrayBuffer[(String,String)]()
       }
     }
   } 
   
   private def getHtmlDataAttributesIds(token: Token, dataAttributesPrefix: String) : ArrayBuffer[(String,String)] = {
    val dataAttributes = ArrayBuffer[(String,String)]()
  
    token.get(fullName) match {
      case Some(name) => { dataAttributes +=  ((dataAttributesPrefix + "fullname",s""""""" + name.toString + s""""""")) } 
      case None => {}
    } 
    
    token.get(typeId) match {
      case Some(id) => { dataAttributes += ((dataAttributesPrefix + "typeid",s""""""" + id.toString + s""""""")) }
      case None => {}
    }
     
    token.get(internalTokenId) match {
      case Some(id) => { dataAttributes += ((dataAttributesPrefix + "internaltokenid",s""""""" + id.toString + s""""""")) }
      case None => {}
    } 
    
    token.get(whereUsed_WithinFileTokenIdSrcPos) match {
      case Some(srcPos) => { val tokenIds=srcPos.map(e=> "#T" + e.tokenId ).mkString("",",","") 
        dataAttributes += ((dataAttributesPrefix + "whereusedinfile",s""""""" + tokenIds + s""""""")) }
      case None => {}
    } 
    
    token.get(implicitConversion_indicator) match {
      case Some(name) => { 
        dataAttributes += ((dataAttributesPrefix + "implicitconversion",s""""""" + true + s""""""")) 
        // Add others: Name and Tokenbased source position 
      }
      case None => { } 
    }
    
    dataAttributes
   }
   
   private def getHtmlDataAttributesLiterals(token: Token, dataAttributesPrefix: String) : ArrayBuffer[(String,String)] = {
     
   val dataAttributes = ArrayBuffer[(String,String)]()
  
    token.get(implicitConversion_indicator) match {
      case Some(name) => { 
        dataAttributes += ((dataAttributesPrefix + "implicitconversion",s""""""" + true + s""""""")) 
        // Add others: Name and Tokenbased source position 
      }
      case None => { } 
    }
    
    dataAttributes
     
   }
   
}
