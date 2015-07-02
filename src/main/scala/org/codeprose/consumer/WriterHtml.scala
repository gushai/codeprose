package org.codeprose.consumer

import scalariform.lexer.Token
import scalariform.lexer.Tokens
import java.io.File
import org.codeprose.util.FileUtil
import scala.collection.immutable.ListMap
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.LoggerMacro
import com.typesafe.scalalogging.LazyLogging



class WriterHtml(outputPath: File)
extends Consumer with LazyLogging {
 
  
	def generateOutput(
			info: scala.collection.mutable.ArrayBuffer[(java.io.File, scala.collection.mutable.Map[Int,(Token,List[(String, String)] )])]
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
			info: scala.collection.mutable.Map[Int,(Token, List[(String, String)])]) : Unit = {

      logger.info("Processing: \t" + srcFile)
			val infoSorted = ListMap(info.toSeq.sortBy(_._1):_*)

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

	private def generateHtmlEntries(infoSorted: ListMap[Int, (Token, List[(String, String)])]) : Iterable[String] = {

			val htmlEntries = infoSorted.map(e => {
				val token = e._2._1
				val tokenProp = e._2._2

						if(Tokens.KEYWORDS.contains(token.tokenType)){
							handleKeywords(token,tokenProp)             
						} else if(Tokens.LITERALS.contains(token.tokenType)) {
							handleLiterals(token,tokenProp)
						} else if(Tokens.COMMENTS.contains(token.tokenType)) {
              handleComments(token, tokenProp)
            }
						else {              
							token.tokenType match {						          
							case Tokens.VARID => {
								if (tokenProp.length==0){
									s""""""+ token.rawText
								} else {
                  
									    val typ=tokenProp.filter(e => e._1.equals("DECLAS"))(0)._2.substring(1)
                      val name =tokenProp.filter(e => e._1.equals("NAME"))(0)._2                      
											val typId = tokenProp.filter(e => e._1.equals("TYPEID"))(0)._2
											
											s"""<span class="$typ" title="Name: $name Declared-As: $typ, ($typId)">""" + token.rawText + "</span>"  
								}        
							}                         
							case _ => {
								token.rawText 
							}
							}
						}   
			}) 
			htmlEntries
	}

  private def handleComments(token: Token, tokenProp: List[(String, String)]) : String = {
    import org.codeprose.consumer.util.MarkdownConverter
    import org.codeprose.consumer.util.CommentUtil
    token.tokenType match{ 
      case Tokens.MULTILINE_COMMENT => {
                val s = if(CommentUtil.isScalaDocComment(token.rawText)){
                  handleCommentsScalaDoc(token)
                } else {
                  s"""\n<div class="textbox">""" + MarkdownConverter.apply(CommentUtil.cleanMultilineComment(token.rawText)) + "\n</div>"
                }
                s
              }        
       case _ => {            
                s"""<span class="comment">""" + token.rawText + "</span>"
              }        
    }
  }
  
  private def handleCommentsScalaDoc(token: Token) : String = {
      // TODO: Include proper handling of Scaladoc comments
      s"""<span class="scaladoc">""" + token.rawText + "</span>"
  }
  
	private def handleKeywords(token: Token, tokenProp: List[(String, String)]) : String = {
			token.tokenType match {
			case Tokens.ABSTRACT => {
				s"""<span class="keyword" title="Scala keyword: ABSTRACT">""" + token.rawText + "</span>" 
			}
			case Tokens.CASE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.CATCH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.CLASS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.DEF => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.DO => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.ELSE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.EXTENDS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.FINAL => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.FINALLY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.FOR => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.FORSOME => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.IF => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.IMPLICIT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.IMPORT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.LAZY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.MATCH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.NEW => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.OBJECT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.OVERRIDE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.PACKAGE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.PRIVATE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.PROTECTED => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.RETURN => {
				s"""<span class="return" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.SEALED => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.SUPER => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.THIS => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.THROW => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.TRAIT => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.TRY => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.TYPE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.VAL => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.VAR => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.WHILE => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.WITH => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>" 
			}
			case Tokens.YIELD => {
				s"""<span class="keyword" title="Scala keyword">""" + token.rawText + "</span>"
			}
			}
	}

	private def combineHtmlEntriesInContainer(
			infoSorted: ListMap[Int, (Token, List[(String, String)])],
			htmlEntries: Iterable[String]
			) 
			: scala.collection.mutable.ArrayBuffer[String] = {
      import org.codeprose.consumer.util.CommentUtil     
			val combinedHtmlEntries = (infoSorted.map(e=>if(e._2._1.tokenType == Tokens.MULTILINE_COMMENT && !CommentUtil.isScalaDocComment(e._2._1.rawText)) true else false).toList zip htmlEntries)

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
  
	def handleLiterals(token: Token, tokenProp: List[(String, String)]) : String = {
		token.tokenType match {
		case Tokens.CHARACTER_LITERAL => {
			s"""<span class="stringLiteral" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.INTEGER_LITERAL => {
			s"""<span class="numberLiteral" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.FLOATING_POINT_LITERAL => {
			s"""<span class="numberLiteral" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.STRING_LITERAL => {
			s"""<span class="stringLiteral" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.STRING_PART => {
			s"""<span class="stringLiteral" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.SYMBOL_LITERAL => {
			s"""<span class="literal" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.TRUE => {
			s"""<span class="keyword" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.FALSE => {
			s"""<span class="keyword" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
		}
		case Tokens.NULL => {
			s"""<span class="keyword" title="Name: """ + token.tokenType.toString +s"""">""" + token.rawText + "</span>"
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
