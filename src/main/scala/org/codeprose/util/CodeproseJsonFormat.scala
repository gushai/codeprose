package org.codeprose.util

import spray.json._

import org.codeprose.api.TypeInformation
import org.codeprose.api.ERangePositionWithTokenId
import org.codeprose.api.SourcePositionLinkWithCodeSample

import org.codeprose.api.TypeInspectInfo
import org.codeprose.api.TypeInfo
import org.codeprose.api.BasicTypeInfo
import org.codeprose.api.ArrowTypeInfo


object CodeproseJsonFormat extends DefaultJsonProtocol {
  implicit val typeInformationFormat = jsonFormat3(TypeInformation)
  //implicit val ERangePositionWithTokenIdsFormat = jsonFormat5(ERangePositionWithTokenId)
  implicit val SourcePositionLinkWithCodeSampleFormat = jsonFormat4(SourcePositionLinkWithCodeSample)
  
  //implicit val TypeInfoFormat = jsonFormat8(TypeInfo)
  
  // TODO: 
  // continue with https://github.com/spray/spray-json#jsonformats-for-recursive-types
  // Example: http://stackoverflow.com/questions/16431545/how-can-provide-jsonformats-for-case-class-that-references-itself
    
//  implicit val BasicTypeInfoFormat = jsonFormat8(BasicTypeInfo)
//  implicit val ArrowTypeInfoFormat = jsonFormat4(ArrowTypeInfo)
//  implicit val TypeInspectInfoFormat = jsonFormat3(TypeInspectInfo)
    
}