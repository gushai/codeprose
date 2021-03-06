package org.codeprose.consumer.scalalang

import scala.collection.mutable.ArrayBuffer
import org.codeprose.api.Token
import org.codeprose.api.scalalang._
import org.codeprose.api.scalalang.ScalaLang._
import org.codeprose.api.scalalang.ScalaLang.Tokens
import org.codeprose.consumer.util.CommentUtil
import org.codeprose.consumer.util.MarkdownConverter


/**
 * Maps from token to output entry.
 */
trait TokenToOutputEntry {
  // Put java script elements that are triggered by individual tokens here.
  val scriptElements = ArrayBuffer[String]()
  /**
   * Returns tuple of the html elements to wrap the token text in.
   *  Tuple._1 -> html code before token text
   *  Tuple._2 -> html code after token text
   *  
   * For example.
   * ("<a href='somelink.html'>","</a>")
   *  
   * @param token Token
   * @return      Tuple of strings with html code to be 
   *              put before and after the token text in the output.
   */
  def getTokenEntryWrapper(token: Token) : (String,String)
}


/**
 * Provides the HTML elements for a token.text.
 */
class TokenToOutputEntryHtml(htmlOutputContext: HtmlOutputContext) 
    extends HtmlDataAttributeGen(htmlOutputContext) with TokenToOutputEntry { 
       
  /**
   * Returns wrapper for token.text.
   * @param token Token
   * @return      Wrapper for token.text in output with usage as
   *              (beg,end) : (String,String) beg + token.text + end 
   */
  def getTokenEntryWrapper(token: Token) : (String,String) = {
     
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
      
	  import org.codeprose.api.scalalang.ScalaLang.Tokens._

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
	  import org.codeprose.api.scalalang.ScalaLang.Tokens._

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

	  import org.codeprose.api.scalalang.ScalaLang.Tokens._ 

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
	  
    import ScalaLang._
    
    val rawTitleElements = List(token(fullName).getOrElse(""),
                                "TokenType: " + token(tokenType).getOrElse(""),
                                "Offset: " + token.offset).mkString("\n")
    
    val title = s""" title="""" + rawTitleElements + s"""" """
    
		val domElementId = token(internalTokenId) match {
			  case Some(id) => {s""" id="T$id" """}
			  case None => s""" """
	  }
    
	  val (linkToDeclaredAt_beg,linkToDeclaredAt_end) = if(token(declaredAt).isDefined){

		  val srcPos = token(declaredAt).get

      htmlOutputContext.getRelativeOutputFilename(srcPos.filename) match {
        case Some(relLinkToOtherSrcOutputFile) => {
          val tId = srcPos.tokenId
          val link = ".." + relLinkToOtherSrcOutputFile + "#" + "T" + tId.toString
          (s"""<a href="$link" class="in-code">""","</a>")   
        } 
        case None => {
          ("","")
        }
      }
	  } else { ("","") }

	  val spanClass = token(symbolDesignation) match {
	    case Some(spClass) => {s""" class="$spClass" """}
	    case None => " "
	  }

	  val dataAttributes = getHtmlDataAttributes(token).map(e=> e._1 + "=" + e._2).mkString(" "," "," ")

    val spanElementBeg = s"""<span""" + spanClass + domElementId + s"""$dataAttributes $title>"""
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
    
}



