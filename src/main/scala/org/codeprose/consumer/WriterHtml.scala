package org.codeprose.consumer

import java.io.File

import org.codeprose.api.ScalaLang._
import org.codeprose.api.Token
import org.codeprose.consumer.util.CommentUtil
import org.codeprose.consumer.util.MarkdownConverter
import org.codeprose.util.FileUtil
import org.codeprose.util.StringUtil

import com.typesafe.scalalogging.LazyLogging



trait WriterContext {
  val verbose: Boolean
}

class WriterHtmlContext(
    val verbose: Boolean
    ) extends WriterContext {    
}


class WriterHtml(outputPath: File)(implicit c: WriterHtmlContext)
extends Consumer with LazyLogging {
 
  
	def generateOutput(
			info: scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[Token])]
			): Unit = {
    
      import org.codeprose.util.StringUtil
			val filenamesShorted = StringUtil.getUniqueShortFileNames(info.map(e => e._1.getAbsolutePath).toList)
			val outputFilenames = filenamesShorted.map(s => outputPath + "/content/" + s.replace("/","_") + ".html")
      
      logger.info("Generating output ...")					

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
      
			val htmlFrame = new HtmlContext(srcFile.getAbsolutePath(),getPackageInformationForFile(srcFile))

			val htmlEntries = generateHtmlEntries(info)

			val outputArray = combineHtmlEntriesInContainer(info,htmlEntries)

			FileUtil.writeToFile(outputFile,htmlFrame.begin + outputArray.mkString("") + htmlFrame.end)    
	}

  /// TODO: Use meta file information.
  private def getPackageInformationForFile(file: File) : String = {
    "org.codeprose.rational"
  }
  
	private def generateHtmlEntries(
			infoSorted: scala.collection.mutable.ArrayBuffer[Token]) : Iterable[String] = {

			val htmlEntries = infoSorted.map(token => {

				import org.codeprose.api.ScalaLang._

				val tokenTyp = token(tokenType)
				tokenTyp match {
				case Some(tt) => {
					if(tt.isKeyword){
						handleKeywords(token,tt)             
					} else if(tt.isLiteral) {
						handleLiterals(token,tt)
					} else if(tt.isComment) {
						handleComments(token,tt)
					}
					else {              
						tt match {                      
						case Tokens.VARID => {
                 
                  // Text to be printed
                  val tText = token.text
                
                  // Fill title information
                  val tInfo = token.toString().replace(",", ",\n")
                  
                  // Determine span class
                  val spanClass = token(symbolDesignation)
                    
                  if(spanClass!=None){
                    s"""<span class="""" + spanClass.get + s"""" title="$tInfo">""" + tText + s"""</span>"""
                  } else {
                    s"""<span title="$tInfo">""" + tText + s"""</span>"""
                  }
                  
						}                         
						case _ => {
							  token.text 
						  }
						}
					}
				} 
				case _ => {
			  		token.text 
			  	} 
				}


			}) 
			htmlEntries
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
        s"""<span class="comment">""" + token.text + "</span>"
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
  
	def handleLiterals(token: Token, tokenTyp: org.codeprose.api.ScalaLang.ScalaTokenType) : String = {
    
    import org.codeprose.api.ScalaLang._
    import org.codeprose.api.ScalaLang.Tokens._
    
		tokenTyp match {
		case CHARACTER_LITERAL => {
			s"""<span class="stringLiteral" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case INTEGER_LITERAL => {
			s"""<span class="numberLiteral" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case FLOATING_POINT_LITERAL => {
			s"""<span class="numberLiteral" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case STRING_LITERAL => {
			s"""<span class="stringLiteral" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case STRING_PART => {
			s"""<span class="stringLiteral" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case SYMBOL_LITERAL => {
			s"""<span class="literal" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case TRUE => {
			s"""<span class="keyword" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case FALSE => {
			s"""<span class="keyword" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}
		case NULL => {
			s"""<span class="keyword" title="Name: """ + token(tokenType).get.toString +s"""">""" + token.text + "</span>"
		}

		}
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
