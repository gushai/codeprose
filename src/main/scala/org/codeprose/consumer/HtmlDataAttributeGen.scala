package org.codeprose.consumer

import scala.collection.mutable.ArrayBuffer
import org.codeprose.api._
import org.codeprose.api.scalalang._
import org.codeprose.api.scalalang.ScalaLang.ScalaTokenType
import org.codeprose.api.scalalang.ScalaLang.Tokens
import spray.json._
import org.codeprose.util.CodeproseJsonFormat._


class HtmlDataAttributeGen(htmlOutputContext: HtmlOutputContext) {
  
  val htmlDataAttributePrefix  = "data-cp-" 
  
  /**
   * Creates html data attributes for a token.
   * @param token Token to process.
   * @return      ArrayBuffer of "(key,value)"
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
   * @return      (key,value) : (String,String)
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
    
    // Implicit indicators
    token(implicitConversion_indicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitconversion",s""""""" + true + s""""""")) }
      case None => {} 
    }
    
    token(implicitConversion_ids) match {
      case Some(ids) => { val idsJson = ids.toJson.compactPrint 
        dataAttributes += ((htmlDataAttributePrefix + "implicitconversionids",s""""""" + idsJson + s""""""")) 
      } 
      case None => {}
    }
    
    token(implicitParameter_indicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitparameter",s""""""" + true + s""""""")) }
      case None => {} 
    }
    
    token(implicitParameter_ids) match {
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
    
    
    //    token(whereUsed_WithinFileTokenIdSrcPos) match {
    //      case Some(srcPos) => { val tokenIds=srcPos.map(e=> "#T" + e.tokenId ).mkString("",",","") 
    //        dataAttributes += ((dataAttributesPrefix + "whereusedinfile",s""""""" + tokenIds + s""""""")) }
    //      case None => {}
    //    } 
    //           

    //    
    //   token(implicitConversion_indicator) match {
    //      case Some(name) => { 
    //        dataAttributes += ((dataAttributesPrefix + "implicitconversion",s""""""" + true + s""""""")) 
    //      }
    //      case None => { } 
    //    }
    //    
    //    token(implicitConversion_fullName) match {
    //      case Some(name) => { 
    //        dataAttributes += ((dataAttributesPrefix + "implicitconversionfullname",s""""""" + name + s""""""")) 
    //      }
    //      case None => { } 
    //    }
    //
    //    token(implicitConversion_sourcePosition) match {
    //      case Some(srcPos) => { 
    //        // TODO: ADD token ID
    //        val link = "." + getRelativeOutputFilenameFromOriginalFile(srcPos.filename) + "#" + "T" + ""
    //        dataAttributes += ((dataAttributesPrefix + "implicitconversiondeclaredat",s""""""" + link + s"""""""))          
    //         
    // 
    //      }
    //      case None => { } 
    //    }
    //    
    //    token(implicitParameter_indicator) match {
    //      case Some(name) => { 
    //        dataAttributes += ((dataAttributesPrefix + "implicitparameter",s""""""" + true + s""""""")) 
    //      }
    //      case None => { } 
    //    }
    //    
    //    token(implicitParameter_fullName) match {
    //      case Some(name) => { 
    //        dataAttributes += ((dataAttributesPrefix + "implicitparameterfullname",s""""""" + name + s""""""")) 
    //      }
    //      case None => { } 
    //    }
    //
    //    token(implicitParameter_sourcePosition) match {
    //      case Some(srcPos) => { 
    //        // TODO: ADD token ID
    //        val link = "." + getRelativeOutputFilenameFromOriginalFile(srcPos.filename) + "#" + "T" + ""
    //        dataAttributes += ((dataAttributesPrefix + "implicitparameterdeclaredat",s""""""" + link + s"""""""))          
    //         
    // 
    //      }
    //      case None => { } 
    //    }


    dataAttributes += ((htmlDataAttributePrefix + "tooltipdisplay",s""""""" + true + s"""""""))


        dataAttributes
  }

  private def getHtmlDataAttributesLiterals(token: Token) : ArrayBuffer[(String,String)] = {

    val dataAttributes = ArrayBuffer[(String,String)]()

        import ScalaLang._

    token(tokenType) match {
        case Some(name) => { 
          dataAttributes += ((htmlDataAttributePrefix + "fullname",s""""""" + name.toString + s""""""")) 
        }
        case None => { } 
    }

    // Implicit indicators

    token(implicitConversion_indicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitconversion",s""""""" + true + s""""""")) }
      case None => { } 
    }
    
    token(implicitConversion_ids) match {
      case Some(ids) => { val idsJson = ids.toJson.compactPrint 
        dataAttributes += ((htmlDataAttributePrefix + "implicitconversionids",s""""""" + idsJson + s""""""")) 
      } 
      case None => {}
    }
    
    token(implicitParameter_indicator) match {
      case Some(bool) => { dataAttributes += ((htmlDataAttributePrefix + "implicitparameter",s""""""" + true + s""""""")) }
      case None => { } 
    }
    
    token(implicitParameter_ids) match {
      case Some(ids) => { val idsJson = ids.toJson.compactPrint 
        dataAttributes += ((htmlDataAttributePrefix + "implicitparameterids",s""""""" + idsJson + s""""""")) 
      } 
      case None => {}
    }

    // Tooltip indicator
    dataAttributes += ((htmlDataAttributePrefix + "tooltipdisplay",s""""""" + true + s"""""""))

    dataAttributes

  }
  
}
