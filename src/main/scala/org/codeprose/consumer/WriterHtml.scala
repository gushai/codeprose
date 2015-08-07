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
      
      logger.info("Generating output ...")					

      // Output context
      setupOutputContext(outputPath)
      
      generateIndexFile(info.map(e => e._1.getAbsolutePath()).toList,filenamesShorted,outputFilenames)
      
			var idx=0
			for(i<-info){     
			  generateOutputFile(new File(outputFilenames(idx)),i._1,i._2)
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
			info: scala.collection.mutable.ArrayBuffer[Token]
      ) : Unit = {

      logger.info("Individual pages ... ")
      if(c.verbose)
        logger.info(srcFile + " ... ")
      
			val htmlContext = new HtmlContext(srcFile.getAbsolutePath(),getPackageInformationForFile(srcFile,info))

			val htmlEntries = generateHtmlEntries(info)(htmlContext)

//			val outputArray = combineHtmlEntriesInContainer(info,htmlEntries)
        val outputArray = htmlEntries
			FileUtil.writeToFile(outputFile,htmlContext.begin + outputArray.mkString("") + htmlContext.end)    
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
    var codeTableClose = true
    val htmlEntries = scala.collection.mutable.ArrayBuffer[String]()
    
    // TODO: Unsave!!!!
    while(idx_toProcess_End<infoSorted.length){
      
      // Find section to process next
      idx_toProcess_End = determineGroupOfTokensToBeProcessedNext(infoSorted,idx_toProcess_End)
     
      // Update codeTableClose?  
      codeTableClose = updateCodeTableClose(infoSorted,idx_toProcess_End)  
      
      // Process subsection of tokens
      val toProcess = infoSorted.slice(idx_toProcess_Beg, idx_toProcess_End).toArray
      //print("\n------------------\n" + toProcess.map(t=>t.text).mkString("") )
     
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
    true
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
      
      currentLineUpdate += toProcess(0).text.count(_ == '\n') 
      
      val (wrap_beg,wrap_end) = TokenToOutputEntry.getTokenEntry(toProcess(0))
    
      Array(htmlContext.textTable_getBegin() + 
      htmlContext.textTable_getEntry("", wrap_beg+ wrap_end) + 
      htmlContext.textTable_getEnd())
       
      
    } else {
     
      
      val (table_beg,table_end) = if(!codeTableOpen && !codeTableClose){
        (htmlContext.codeTable_getBegin(),"")
      } else if (!codeTableOpen && codeTableClose){
        (htmlContext.codeTable_getBegin(), htmlContext.codeTable_getEnd())
      } else if (codeTableOpen && !codeTableClose){
        ("","")
      } else {
        ("",htmlContext.codeTable_getEnd())
      }
      
      // Get html token wrapper
      val wrapper = toProcess.map(t=>{
        TokenToOutputEntry.getTokenEntry(t)
      })
      
      
     
      
      // Determine (a) all tokens in one line (b) parts of tokens to be split over lines
      val totalLineUpdate = toProcess.map(t=>t.text).mkString("").count(_ == '\n')
      
      val tmp = ArrayBuffer[String]()
      var str = ""
      
      for(i<- 0 to (toProcess.length-1)){
        val t = toProcess(i)
        val (beg,end) = wrapper(i)
        
        //print(str)
        
        if(t.text.contains('\n')){
          
          if(t(tokenType).isDefined && t(tokenType).get == Tokens.WS){
            val numNewLines = t.text.count(_ == '\n')
            val idx_FirstNewLine = t.text.indexOf("\n")
            val idx_LastNewLine = t.text.lastIndexOf("\n")
            str = str + (beg + t.text.slice(0,idx_FirstNewLine) + "aa" + end)
            tmp.append(str) 
            
            if(numNewLines>2){
              for(k<- 3 to numNewLines){
                tmp.append("empty")
              }
            } 
            
            if(idx_LastNewLine < (t.text.length-1)){
              str = (beg + t.text.slice(idx_LastNewLine+1,t.text.length) +"[XX]" + end)
            }
            
          } else {
            // TODO: Improve for multiline STRING_LITERALS etc.
            str = str + (beg + t.text + end)
            tmp.append(str) 
            str = ""
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

  
  private def handleComments(token: Token, tokenTyp: org.codeprose.api.ScalaLang.ScalaTokenType) : String = {
    import org.codeprose.consumer.util.MarkdownConverter
    import org.codeprose.consumer.util.CommentUtil
    import org.codeprose.api.ScalaLang._
    
    
    tokenTyp match{ 
      case Tokens.MULTILINE_COMMENT => {
        val s = if(CommentUtil.isScalaDocComment(token.text)){
          handleCommentsScalaDoc(token)
        } else {
          s"""\n<div class="textbox">""" + MarkdownConverter.apply(CommentUtil.cleanMultilineComment(token.text)) + "\n</div>"
        }
        s
       }        
       case _ => {          
        val tInfo = token.toString().replace(";",";\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
        s"""<span class="comment" title="""" + tInfo + s"""">""" + token.text + "</span>"
       }        
    }
  }
  
  private def handleCommentsScalaDoc(token: Token) : String = {
      s"""<span class="scaladoc" title="ScalaDoc Comment">""" + token.text + "</span>"
  }
  
	private def handleKeywords(
      token: Token, 
      tokenTyp: org.codeprose.api.ScalaLang.ScalaTokenType
      ) : String = {
  
      import org.codeprose.api.ScalaLang._
      import org.codeprose.api.ScalaLang.Tokens._
    
      
			tokenTyp match {
			case ABSTRACT => {
				s"""<span class="keyword" title="Scala keyword: ABSTRACT">""" + token.text + "</span>" 
			}
			case CASE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case CATCH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case CLASS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case DEF => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case DO => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ELSE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case EXTENDS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case FINAL => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case FINALLY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case FOR => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case FORSOME => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case IF => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case IMPLICIT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case IMPORT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case LAZY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case MATCH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case NEW => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case OBJECT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case OVERRIDE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case PACKAGE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case PRIVATE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case PROTECTED => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case RETURN => {
				s"""<span class="return" title="Scala keyword">""" + token.text + "</span>" 
			}
			case SEALED => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case SUPER => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case THIS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case THROW => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case TRAIT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case TRY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case TYPE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case VAL => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case VAR => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case WHILE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case WITH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case YIELD => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>"
			}
			}
	}

	private def combineHtmlEntriesInContainer(
			infoSorted: scala.collection.mutable.ArrayBuffer[Token],
			htmlEntries: Iterable[String]
			) 
			: scala.collection.mutable.ArrayBuffer[String] = {
      import org.codeprose.consumer.util.CommentUtil     
      import org.codeprose.api.ScalaLang._
     
   
      val combinedHtmlEntries = (
          infoSorted.map(
              t => 
                if(t(tokenType) match { 
                  case Some(tt) => { tt == Tokens.MULTILINE_COMMENT && !CommentUtil.isScalaDocComment(t.text) } 
                  case None => { false }
                  }
                ) 
                  true 
                else 
                  false
              )
          .toList zip htmlEntries
          )

					val outputArray = scala.collection.mutable.ArrayBuffer[String]()

					var i=0
					var open = false
					while(i<combinedHtmlEntries.length){
						if(combinedHtmlEntries(i)._1){
							if(open){
								outputArray += "</pre>\n</div>\n" + combinedHtmlEntries(i)._2
										open = false
							} else {
								outputArray += combinedHtmlEntries(i)._2
							}
						} else {
							if (open) {
								outputArray += combinedHtmlEntries(i)._2
							} else {            
								outputArray += s"""\n\n<div class="codebox"><pre>""" + combinedHtmlEntries(i)._2            
										open=true
							}        
						}
						i+=1 
					}
			if(open) 
				outputArray += "\n</pre>\n</div>"

				outputArray
	}
  
	private def handleLiterals(token: Token, tokenTyp: org.codeprose.api.ScalaLang.ScalaTokenType) : String = {
    
    import org.codeprose.api.ScalaLang._
    import org.codeprose.api.ScalaLang.Tokens._
    
    val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
               
    
		tokenTyp match {
		case CHARACTER_LITERAL => {
			s"""<span class="stringLiteral" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case INTEGER_LITERAL => {
			s"""<span class="numberLiteral" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case FLOATING_POINT_LITERAL => {
			s"""<span class="numberLiteral" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case STRING_LITERAL => {
			s"""<span class="stringLiteral" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case STRING_PART => {
			s"""<span class="stringLiteral" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case SYMBOL_LITERAL => {
			s"""<span class="literal" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case TRUE => {
			s"""<span class="keyword" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case FALSE => {
			s"""<span class="keyword" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}
		case NULL => {
			s"""<span class="keyword" title="""" + tInfo +s"""">""" + token.text + "</span>"
		}

		}
	}

  private def setupOutputContext(outputPath: File) : Unit = {
    val setter = new OutputContextSetter(outputPath)
    setter.setFolderStructure(c.outputFolders)
    c.filesToCoy.foreach({ f => setter.copyResource(f.base, new File(outputPath,f.target)) }) 
	}
}





class HtmlContext(filename: String, packag: String) {

	    val fileNameWithoutPath =  filename.slice(filename.lastIndexOf("/")+1, filename.length())  
			val fileNameWithoutPathAndEnding = fileNameWithoutPath.slice(0,fileNameWithoutPath.lastIndexOf("."))		
			val fileNameEnding = fileNameWithoutPath.slice(fileNameWithoutPath.lastIndexOf(".")+1,fileNameWithoutPath.length)

			val begin =s"""<!doctype HTML>
			<html lang="en">
			<head>
			<meta http-equiv="content-type" content="text/html; charset=utf-8" />
			<link rel="stylesheet" type="text/css" href="../style/style.css" media="screen" />
			<title>$fileNameWithoutPath</title>
			<script type="text/javascript">
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

			val end  = s"""<div class="footer">generated by codeprose. help and support on <a href="http://github.com/sth/codeprose" target="blank">github</a>.</div>
			</div></div></body></html>"""

      
      def textTable_getBegin() : String = { s"""<table class="table-text">""" } 

      def textTable_getEnd() : String = { s"""</table>\n\n""" } 
      
      def textTable_getEntry(comment: String, mainText: String) : String = {
        s"""<tr>
<td class="table-text-comment">$comment</td>
<td class="table-text-text">$mainText</td>
</tr>"""
      } 

      def codeTable_getBegin() : String = { """<table class="table-code">""" }
      
      
      def codeTable_getEntry(lineNumber: Int, comment: String, code: String) : String = {
        val lineNumStr = lineNumber.toString()
        s"""<tr>
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
					
      var entries = (originalFilenames zip ( labels zip links)).map{e => (e._1,e._2._1,e._2._2)}

			frameBeg+ beg + "<ul>" + entries.map({e => 
			s"""<li><a href="""" + 
			e._3 + s"""" title="Originial filename:""" +e._1 + s"""">""" +
			e._2 + s"""</a></li>"""}).mkString("\n") + "</ul>\n" + frameEnd  
	}

}


object TokenToOutputEntry {
  
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
          }
          else {        
            
            tt match {
              case Tokens.VARID => {
                handleVARID(token)
              }
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
    
    tt match {
    case CHARACTER_LITERAL => {
      (s"""<span class="stringLiteral" title="""" + tInfo +s"""">""", "</span>")
    }
    case INTEGER_LITERAL => {
      (s"""<span class="numberLiteral" title="""" + tInfo +s"""">""", "</span>")
    }
    case FLOATING_POINT_LITERAL => {
      (s"""<span class="numberLiteral" title="""" + tInfo +s"""">""", "</span>")
    }
    case STRING_LITERAL => {
      (s"""<span class="stringLiteral" title="""" + tInfo +s"""">""", "</span>")
    }
    case STRING_PART => {
      (s"""<span class="stringLiteral" title="""" + tInfo +s"""">""", "</span>")
    }
    case SYMBOL_LITERAL => {
      (s"""<span class="literal" title="""" + tInfo +s"""">""", "</span>")
    }
    case TRUE => {
      (s"""<span class="keyword" title="""" + tInfo +s"""">""", "</span>")
    }
    case FALSE => {
      (s"""<span class="keyword" title="""" + tInfo +s"""">""", "</span>")
    }
    case NULL => {
      (s"""<span class="keyword" title="""" + tInfo +s"""">""", "</span>")
    }

    }
   }
   
  private def handleVARID(token: Token) : (String,String) = {

		// Fill title information
		val tInfo = token.toString().replace(";", ";\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length

		// Determine span class
    token(symbolDesignation) match {
      case Some(spanClass) => {
        (s"""<span class="""" + spanClass + s"""" title="$tInfo">""",s"""</span>""")
      } 
      case None => {
        (s"""<span title="$tInfo">""", s"""</span>""")
      }
    } 
   }
   
   private def handleWS(token: Token) : (String,String) = {
     
     ("","")
   }
   
}

//val tokenTyp = token(tokenType)
//        tokenTyp match {
//        case Some(tt) => {
//          if(tt.isKeyword){
//            handleKeywords(token,tt)             
//          } else if(tt.isLiteral) {
//            handleLiterals(token,tt)
//          } else if(tt.isComment) {
//            handleComments(token,tt)
//          }
//          else {              
//            tt match {                      
//            case Tokens.VARID => {
//                 
//                  // Text to be printed
//                  val tText = token.text
//                
//                  // Fill title information
//                  val tInfo = token.toString().replace(";", ";\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
//                  
//                  // Determine span class
//                  val spanClass = token(symbolDesignation)
//                    
//                  if(spanClass!=None){
//                    s"""<span class="""" + spanClass.get + s"""" title="$tInfo">""" + tText + s"""</span>"""
//                  } else {
//                    s"""<span title="$tInfo">""" + tText + s"""</span>"""
//                  }
//                  
//            }
//            case Tokens.WS => {
//              token.text
//            }
//            case _ => {
//              val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
//              s"""<span title=""""+ tInfo + s"""">""" + token.text + s"""</span>"""
//              }
//            }
//          }
//        } 
//        case _ => {
//            token.text
//          } 
//        }
//
//
//      })