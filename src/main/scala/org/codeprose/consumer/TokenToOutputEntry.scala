package org.codeprose.consumer

import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token
import org.codeprose.api.ScalaLang.ScalaTokenType
import org.codeprose.api.ScalaLang.Tokens


/**
 * Element to map from token to output entry.
 * @param token Token
 * @return      
 */
trait TokenToOutputEntry {
  val scriptElements = ArrayBuffer[String]()
  def getTokenEntryWrapper(token: Token) : (String,String)
}

//class TokenToOutputEntryHtml(val filenamesOriginalToOutputNames: Array[(String,String)]) extends TokenToOutputEntry {
class TokenToOutputEntryHtml()(implicit hmtlOutputContext: HtmlOutputContext) extends TokenToOutputEntry with HtmlDataAttributeGen { 
  
   
  
  /**
   * Returns wrapper for tocken.text.
   * @param token Token
   * @return      Wrapper for token.text in output with usage as
   *              (beg,end) : (String,String) beg + token.text + end 
   */
  def getTokenEntryWrapper(token: Token) : (String,String) = {
     
     import org.codeprose.api.ScalaLang._
     
     token(tokenType) match {
       case Some(tt) => {
          val wrapper = if(tt.isKeyword){
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
          wrapper
       }
       case None => {
         ("","")
       }
       
     }
      
  }
  
  
  /**
   * Returns wrapper for Comments.
   * @param token     Token to process.
   * @param tokenTp   ScalaTokenType
   * @return          Wrapper
   */
  private def handleComments(token: Token, tokenTpe: ScalaTokenType) : (String,String) = {
       
    import org.codeprose.consumer.util.MarkdownConverter
    import org.codeprose.consumer.util.CommentUtil

    tokenTpe match{ 
       case Tokens.MULTILINE_COMMENT => {
         val s = if(CommentUtil.isScalaDocComment(token.text)){
           (s"""<span class="scaladoc">""","</span>")
         } else {
           (MarkdownConverter.apply(CommentUtil.cleanMultilineComment(token.text)),"") 
         }
         s
       }        
       case _ => {          
         (s"""<span class="comment">""", "</span>")
       }        
       }
   }
  
  
   /**
    * Returns wrapper for KEYWORDS.
    * @param token     Token to process.
    * @param tokenTpe  ScalaTokenType
    * @return          Wrapper
    */
   private def handleKeywords(token: Token, tokenTpe: ScalaTokenType) : (String,String) = {
      
	  import org.codeprose.api.ScalaLang.Tokens._

	  //val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
    
    val title = s""" title="""" + tokenTpe + s"""" """

	  tokenTpe match {
	  case ABSTRACT => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case CASE => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case CATCH => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case CLASS => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case DEF => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case DO => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case ELSE => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case EXTENDS => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case FINAL => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case FINALLY => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case FOR => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case FORSOME => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case IF => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case IMPLICIT => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case IMPORT => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case LAZY => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case MATCH => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case NEW => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case OBJECT => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case OVERRIDE => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case PACKAGE => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case PRIVATE => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case PROTECTED => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case RETURN => {
		  (s"""<span class="return" $title>""", "</span>") 
	  }
	  case SEALED => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case SUPER => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case THIS => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case THROW => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case TRAIT => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case TRY => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case TYPE => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case VAL => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case VAR => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case WHILE => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case WITH => {
		  (s"""<span class="keyword" $title>""", "</span>") 
	  }
	  case YIELD => {
		  (s"""<span class="keyword" $title>""", "</span>")
	  }
	  case _ => {
		  ("","")
	  }
	  }
  }
   
   
  /**
   * Returns the wrapper for Literals.
    * @param token     Token to process.
    * @param tokenTpe  ScalaTokenType
    * @return          Wrapper
   */
  private def handleLiterals(token: Token, tokenTpe: ScalaTokenType) : (String,String) = {
	  import org.codeprose.api.ScalaLang.Tokens._

	  //val tInfo = token.toString().replace(",", ",\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
	  val title = s""" title="""" + tokenTpe + "\nOffset: " + token.offset  + s"""" """

	  val dataAttributes = getHtmlDataAttributes(token).map(e=> e._1 + "=" + e._2).mkString(" "," "," ")


	  tokenTpe match {
	  case CHARACTER_LITERAL => {
		  (s"""<span class="stringLiteral" $dataAttributes $title>""", "</span>")
	  }
	  case INTEGER_LITERAL => {
		  (s"""<span class="numberLiteral" $dataAttributes $title>""", "</span>")
	  }
	  case FLOATING_POINT_LITERAL => {
		  (s"""<span class="numberLiteral" $dataAttributes $title>""", "</span>")
	  }
	  case STRING_LITERAL => {
		  (s"""<span class="stringLiteral" $dataAttributes $title>""", "</span>")
	  }
	  case STRING_PART => {
		  (s"""<span class="stringLiteral" $dataAttributes $title>""", "</span>")
	  }
	  case SYMBOL_LITERAL => {
		  (s"""<span class="literal" $dataAttributes $title>""", "</span>")
	  }
	  case TRUE => {
		  (s"""<span class="keyword" $dataAttributes $title>""", "</span>")
	  }
	  case FALSE => {
		  (s"""<span class="keyword" $dataAttributes $title>""", "</span>")
	  }
	  case NULL => {
		  (s"""<span class="keyword" $dataAttributes $title>""", "</span>")
	  }
	  case _ => {
		  ("","")
	  }

	  }
  }
   
  /**
   * Returns the wrapper for IDs.
   * @param token     Token to process.
   * @param tokenTpe  ScalaTokenType
   * @return          Wrapper
   */
  private def handleIds(token: Token, tokenTpe: ScalaTokenType) : (String,String) = {

	  import org.codeprose.api.ScalaLang.Tokens._ 

	  tokenTpe match {
  	  case VARID => {
  		  handleID(token)
  	  } 
  	  case PLUS => {
  		  handleID(token)
  	  } 
  	  case MINUS => {
  		  handleID(token)
  	  } 
  	  case STAR => {
  		  handleID(token)
  	  } 
  	  case PIPE => {
  		  handleID(token)
  	  } 
  	  case TILDE => {
  		  handleID(token)
  	  } 
  	  case EXCLAMATION => {
  		  handleID(token)
  	  } 
  	  case _ => {
  		  ("","")
  	  }
	  }
  }
   
  /**
   * Returns the wrapper for IDs.
   * @param token     Token to process.
   * @return          Wrapper
   */
  private def handleID(token: Token) : (String, String) = {
	  
    import org.codeprose.api.ScalaLang._
    
    // Fill title information
	  //val tInfo = token.toString().replace(";", ";\n") + ",\n'offset: " + token.offset + ",\n'length: " + token.length
    val rawTitleElements = List(token(fullName).getOrElse(""),
                                "TypeId: " + token(typeId).getOrElse(""),
                                "Offset: " + token.offset).mkString("\n")
    
    val title = s""" title="""" + rawTitleElements + s"""" """
    
    // DOM id 
		val domElementId = token(internalTokenId) match {
			  case Some(id) => {s""" id="T$id" """}
			  case None => s""" """
	  }
    
	  // Link elements to declared_At   
	  val (linkToDeclaredAt_beg,linkToDeclaredAt_end) = if(token(declaredAt_SrcPosWithTokenId).isDefined){

		  val srcPos = token(declaredAt_SrcPosWithTokenId).get

			// Get full file to output translation
			val relLinkToOtherSrcOutputFile = getRelativeLinkToOtherOutputFile(srcPos.filename)
			
      if(relLinkToOtherSrcOutputFile.length>0){
			  val tId = srcPos.tokenId
				val link = relLinkToOtherSrcOutputFile + "#" + "T" + tId.toString
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
	  val dataAttributes = getHtmlDataAttributes(token).map(e=> e._1 + "=" + e._2).mkString(" "," "," ")

			  // Set output 
			  val spanElementBeg = s"""<span""" + spanClass + domElementId + s""" $title + dataAttributes>"""
			  val spanElementEnd = "</span>"

			  if(linkToDeclaredAt_beg.length()!=0){
				  (linkToDeclaredAt_beg + spanElementBeg, spanElementEnd + linkToDeclaredAt_end)
			  } else {
				  (spanElementBeg,spanElementEnd)
			  }

  }
  
  /**
   * Returns wrapper for WS.
   * @param token Token to wrap.
   * @return      Wrapper. 
   */
  private def handleWS(token: Token) : (String,String) = {
	  ("","")
  }

  
  /**
   * Returns relative link to other output file.
   * @param srcFileName Input source file.
   * @return            Link of type "./outputFile.html"
   */
  private def getRelativeLinkToOtherOutputFile(srcFileName: String) : String = {
    
    val absPath = hmtlOutputContext.filenamesOriginalToOutput(srcFileName).getAbsolutePath 
    val idx = absPath.lastIndexOf("/")
    "./" + absPath.slice(idx,absPath.length)
  }
  
//  /**
//   * Maps source file names to output file names.
//   * @param srcFileName Source file name.
//   * @return            Matching output file for link.
//   */
//  def getOutputFilenameFromOriginalFile(srcFile: String) : String = {
//    
//    //hmtlOutputContext.filenamesOriginalToOutputNames.filter( e => e._1 == orgFile).map(e => "./" + e._2)
////    if(paths.size > 0){
////          paths(0)
////        } else {
////          ""
////        }
//	  
//			  
//  }
//
//  def getRelativeOutputFilenameFromOriginalFile(orgFile: String) : String = {
//
//	  val absPath = getOutputFilenameFromOriginalFile(orgFile)
//			  if(absPath.length > 0){
//				  
//			  } else {
//				  ""
//			  }
//
//  }

  
}



