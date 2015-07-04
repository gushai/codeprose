package org.codeprose.consumer


import java.io.File
import org.codeprose.util.FileUtil
import org.codeprose.api.Token
import scala.collection.immutable.ListMap
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.LoggerMacro
import com.typesafe.scalalogging.LazyLogging




class WriterHtml(outputPath: File)
extends Consumer with LazyLogging {
 
  
	def generateOutput(
			info: scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.ArrayBuffer[Token])]
			): Unit = {

			val filenamesShorted = getUniqueShortFileNames(info.map(e => e._1.getAbsolutePath).toArray)
			val outputFilenames = filenamesShorted.map(s => outputPath + "/content/" + s.replace("/","_") + ".html")

      logger.info("Generating output ...")					
			generateIndexFile(info.map(e => e._1.getAbsolutePath()).toArray,filenamesShorted,outputFilenames)
          
			logger.info("Individual pages: ")
			var idx=0
			for(i<-info){     
			  generateOutputFile(new File(outputFilenames(idx)),i._1,i._2)
				idx+=1
      }
			logger.info("Done.")
	}

	private def generateIndexFile(originalFilenames: Array[String], filenamesShortened : Array[String], links: Array[String] ) : Unit = {   
			val outputFilename= new File(outputPath.getAbsolutePath + "/index.html")
      logger.info("Index page: \t" + outputFilename + " ...")      
			val htmlFrame = new HtmlIndexContext()			
			FileUtil.writeToFile(outputFilename,htmlFrame.begin + htmlFrame.getFileListing(originalFilenames,filenamesShortened,links) + htmlFrame.end)      
	} 

	private def generateOutputFile(
			outputFile: File, 
			srcFile: File, 
			info: scala.collection.mutable.ArrayBuffer[Token]) : Unit = {

      logger.info("Processing: \t" + srcFile)
			//val infoSorted = ListMap(info.toSeq.sortBy(_._1):_*)
      val infoSorted = info
      
			val htmlFrame = new HtmlContext(srcFile.getAbsolutePath(),getPackageInformationForFile(srcFile))

			val htmlEntries = generateHtmlEntries(infoSorted)

			val outputArray = combineHtmlEntriesInContainer(infoSorted,htmlEntries)

			FileUtil.writeToFile(outputFile,htmlFrame.begin + outputArray.mkString("") + htmlFrame.end)    
	}

  /// TODO: Use meta file information.
  private def getPackageInformationForFile(file: File) : String = {
    "org.codeprose.rational"
  }
  
	/**
	 * Removes the longest common prefix of all elements
	 */
	private def getUniqueShortFileNames(s: Array[String]) : Array[String] = {
			
      import org.codeprose.util.StringUtil
			val prefixLength = StringUtil.findLongtestCommonPrefix(s.toList).length 
      println
      val idxLastPathSep = s.map { e => e.lastIndexOf("/") }
      val sliceFirstIndex = idxLastPathSep.map(e=> Math.min(prefixLength,e))
      (sliceFirstIndex zip s).map(e => (e._2.slice(e._1,e._2.length)))			
	}

	private def generateHtmlEntries(
			infoSorted: scala.collection.mutable.ArrayBuffer[Token]) : Iterable[String] = {

			val htmlEntries = infoSorted.map(token => {


				import org.codeprose.api.ScalaLang._
				import org.codeprose.api.ScalaTokens

				val tokenTyp = token(tokenType)
				tokenTyp match {
				case Some(tt) => {
					if(ScalaTokens.KEYWORDS.contains(tt)){
						handleKeywords(token,tt)             
					} else if(ScalaTokens.LITERALS.contains(tt)) {
						handleLiterals(token,tt)
					} else if(ScalaTokens.COMMENTS.contains(tt)) {
						handleComments(token,tt)
					}
					else {              
						tt match {                      
						case ScalaTokens.VARID => {

							val typ = token(declaredAs)
									val name = token(fullName)                      
									val typId = token(typeId)

									if (typ == None || name == None || typId == None){
										s""""""+ token.text
									} else {

										val typVal = token(declaredAs).get
												val nameVal = token(fullName).get                 
												val typIdVal = token(typeId).get

												s"""<span class="$typVal" title="Name: $nameVal Declared-As: $typVal, ($typIdVal)">""" + token.text + "</span>"  
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
    import org.codeprose.api.ScalaTokens
    tokenTyp match{ 
      case ScalaTokens.MULTILINE_COMMENT => {
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
      // TODO: Include proper handling of Scaladoc comments
      s"""<span class="scaladoc">""" + token.text + "</span>"
  }
  
	private def handleKeywords(token: Token, tokenTyp: org.codeprose.api.ScalaLang.ScalaTokenType) : String = {
      import org.codeprose.api.ScalaLang._
      import org.codeprose.api.ScalaTokens
    
      
			tokenTyp match {
			case ScalaTokens.ABSTRACT => {
				s"""<span class="keyword" title="Scala keyword: ABSTRACT">""" + token.text + "</span>" 
			}
			case ScalaTokens.CASE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.CATCH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.CLASS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.DEF => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.DO => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.ELSE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.EXTENDS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.FINAL => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.FINALLY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.FOR => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.FORSOME => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.IF => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.IMPLICIT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.IMPORT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.LAZY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.MATCH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.NEW => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.OBJECT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.OVERRIDE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.PACKAGE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.PRIVATE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.PROTECTED => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.RETURN => {
				s"""<span class="return" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.SEALED => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.SUPER => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.THIS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.THROW => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.TRAIT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.TRY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.TYPE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.VAL => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.VAR => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.WHILE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.WITH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.text + "</span>" 
			}
			case ScalaTokens.YIELD => {
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
      import org.codeprose.api.ScalaTokens
      
			val combinedHtmlEntries = (infoSorted.map(t=>if(t(tokenType) == ScalaTokens.MULTILINE_COMMENT && !CommentUtil.isScalaDocComment(t.text)) true else false).toList zip htmlEntries)

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
    import org.codeprose.api.ScalaTokens
    
		tokenTyp match {
		case ScalaTokens.CHARACTER_LITERAL => {
			s"""<span class="stringLiteral" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.INTEGER_LITERAL => {
			s"""<span class="numberLiteral" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.FLOATING_POINT_LITERAL => {
			s"""<span class="numberLiteral" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.STRING_LITERAL => {
			s"""<span class="stringLiteral" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.STRING_PART => {
			s"""<span class="stringLiteral" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.SYMBOL_LITERAL => {
			s"""<span class="literal" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.TRUE => {
			s"""<span class="keyword" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.FALSE => {
			s"""<span class="keyword" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
		}
		case ScalaTokens.NULL => {
			s"""<span class="keyword" title="Name: """ + token(tokenType).toString +s"""">""" + token.text + "</span>"
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

			def getFileListing(originalFilenames: Array[String],labels: Array[String], links: Array[String]): String = {
			// TODO group based on folders
			val frameBeg = s"""<div class="textbox">"""
					val frameEnd = s"""</div>\n"""
					val beg = "<h2>" + "Files" +"</h2>"                 
					
          var entries = (originalFilenames zip ( labels zip links)).map{e => (e._1,e._2._1,e._2._2)}

							frameBeg+ beg + "<ul>" + entries.map({e => 
							s"""<li><a href="""" + 
							e._3 + s"""" title="Originial filename:""" +e._1 + s"""">"""+
							e._2 + s"""</a></li>"""})
							.mkString("\n") + "</ul>\n" + frameEnd  
	}

}
