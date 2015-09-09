package org.codeprose.consumer.scalalang

import scala.collection.mutable.ArrayBuffer
import org.codeprose.api._
import org.codeprose.api.scalalang._
import spray.json._
import org.codeprose.util.CodeproseJsonFormat._
import org.codeprose.api.scalalang.ScalaLang


/**
 * Generates html data attributes for tokens.
 * @param   htmlOutputContext HtmlOutputContext.
 */
class HtmlDataAttributeGen(htmlOutputContext: HtmlOutputContext) {
  
  // Prefix of all data attributes. 
  // Use in jQuery: $(selector).data("cp-ATTRIBUTE"
  val htmlDataAttributePrefix  = "data-cp-" 
  
  /**
   * Creates html data attributes for tokens.
   * @param token Token to process.
   * @return      ArrayBuffer of "(key,value)" with key = data-cp-ATTRIBUTE and value information to save.
   */
  def getHtmlDataAttributes(token: Token) : ArrayBuffer[(String,String)] = {
    token(ScalaLang.tokenType) match {
      case Some(tt) => {
        if(tt.isId){
          getHtmlDataAttributesIds(token)
        } else if (tt.isKeyword) {
          ArrayBuffer[(String,String)]()
        } else if (tt.isXml) {
          ArrayBuffer[(String,String)]()
        } else if (tt.isComment){
          ArrayBuffer[(String,String)]()
        } else if (tt.isLiteral) {
          getHtmlDataAttributesLiterals(token)
        } else {
          ArrayBuffer[(String,String)]()
        }
      }
      case None => {
        ArrayBuffer[(String,String)]()
      }
    }
  } 
   
  /**
   * Generate html data attributes for Ids.
   * @token token Token to process.
   * @return      (key,value) : (String,String) with key = data-cp-ATTRIBUTE and value information to save.
   */
  private def getHtmlDataAttributesIds(token: Token) : ArrayBuffer[(String,String)] = {
    val dataAttributes = ArrayBuffer[(String,String)]()
    
    import ScalaLang._
    
    token(fullName) match {
        case Some(name) => { dataAttributes +=  ((htmlDataAttributePrefix + "fullname",s""""""" + name.toString + s""""""")) } 
        case None => {}
    } 

    token(typeId) match {
      case Some(id) => { dataAttributes += ((htmlDataAttributePrefix + "typeid",s""""""" + id.toString + s""""""")) }
      case None => {}
    }

    token(internalTokenId) match {
      case Some(id) => { dataAttributes += ((htmlDataAttributePrefix + "internaltokenid",s""""""" + id.toString + s""""""")) }
      case None => {}
    }
    
    token(ownerTypeId) match {
      case Some(id) => { dataAttributes += ((htmlDataAttributePrefix + "ownertypeid",s""""""" + id.toString + s""""""")) }
      case None => {}
    }
    
    token(isCallable) match {
      case Some(isCallable) => { dataAttributes += ((htmlDataAttributePrefix + "iscallable",s""""""" + isCallable.toString + s""""""")) }
      case None => {}
    }
    
    token(implicitConversionIndicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitconversion",s""""""" + true + s""""""")) }
      case None => {} 
    }
    
    token(implicitConversionIds) match {
      case Some(ids) => { val idsJson = ids.toJson.compactPrint 
        dataAttributes += ((htmlDataAttributePrefix + "implicitconversionids",s""""""" + idsJson + s""""""")) 
      } 
      case None => {}
    }
    
    token(implicitParameterIndicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitparameter",s""""""" + true + s""""""")) }
      case None => {} 
    }
    
    token(implicitParameterIds) match {
      case Some(ids) => { val idsJson = ids.toJson.compactPrint 
        dataAttributes += ((htmlDataAttributePrefix + "implicitparameterids",s""""""" + idsJson + s""""""")) 
      } 
      case None => {}
    }
    
    token(whereUsedWithInFile) match {
      case Some(srcPos) => { 
    	  val tokenIds=srcPos.map(e=> "#T" + e.tokenId ).mkString("",",","") 
    			dataAttributes += ((htmlDataAttributePrefix + "whereusedinfile",s""""""" + tokenIds + s""""""")) }
      case None => {}
    } 
    
    token(declaredAt) match {
      case Some(srcPos) => {
    	  htmlOutputContext.getRelativeOutputFilename(srcPos.filename) match {
    	    case Some(relLinkToOtherSrcOutputFile) => {
    		    val tId = srcPos.tokenId
    				val link = ".." + relLinkToOtherSrcOutputFile + "#" + "T" + tId.toString
    				dataAttributes += ((htmlDataAttributePrefix + "declaration",s""""""" + link + s"""""""))
    	    } 
    	    case None => {}
    	  }
      } 
      case None => {}
    }

    dataAttributes += ((htmlDataAttributePrefix + "tooltipdisplay",s""""""" + true + s"""""""))
    dataAttributes 
  }

  /**
   * Generate html data attributes for Literals.
   * @token token Token to process.
   * @return      (key,value) : (String,String) with key = data-cp-ATTRIBUTE and value information to save.
   */
  private def getHtmlDataAttributesLiterals(token: Token) : ArrayBuffer[(String,String)] = {

    val dataAttributes = ArrayBuffer[(String,String)]()
    import ScalaLang._

    token(tokenType) match {
        case Some(name) => { 
          dataAttributes += ((htmlDataAttributePrefix + "fullname",s""""""" + name.toString + s""""""")) 
        }
        case None => { } 
    }

    token(implicitConversionIndicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitconversion",s""""""" + true + s""""""")) }
      case None => { } 
    }
    
    token(implicitConversionIds) match {
      case Some(ids) => { val idsJson = ids.toJson.compactPrint 
        dataAttributes += ((htmlDataAttributePrefix + "implicitconversionids",s""""""" + idsJson + s""""""")) 
      } 
      case None => {}
    }
    
    token(implicitParameterIndicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitparameter",s""""""" + true + s""""""")) }
      case None => { } 
    }
    
    token(implicitParameterIds) match {
      case Some(ids) => { val idsJson = ids.toJson.compactPrint 
        dataAttributes += ((htmlDataAttributePrefix + "implicitparameterids",s""""""" + idsJson + s""""""")) 
      } 
      case None => {}
    }

    dataAttributes += ((htmlDataAttributePrefix + "tooltipdisplay",s""""""" + true + s"""""""))
    dataAttributes
  }
}
